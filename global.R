library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(mongolite)
library(glue)
library(rvest)
library(stringr)
library(purrr)
library(curl)
library(tidyr)

if (file.exists("environment.R")) source("environment.R")

tmdb_key <- Sys.getenv("TMDB_KEY")
mongo_uri <- Sys.getenv("MONGOLAB_URI")

tmdb_get <- function(path) {
  uri <- modify_url(
    "https://api.themoviedb.org",
    path = glue("3/{path}"),
    query = list(api_key = tmdb_key)
  )

  resp <- GET(uri)
  jsonlite::fromJSON(content(resp, type = "text", encoding = "UTF-8"))
}

configuration <- tmdb_get("configuration")
secure_base_url <- configuration$images$secure_base_url
poster_sizes <- unlist(configuration$images$poster_sizes)
backdrop_sizes <- unlist(configuration$images$backdrop_sizes)

list_movie_ids <- tmdb_get("list/3492") %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(items.media_type == "movie") %>%
  pull(items.id)

movies_coll <- mongo("movies", url = mongo_uri)
series_coll <- mongo("series", url = mongo_uri)

saved_movie_ids <- movies_coll$distinct("id")

movie_ids_to_save <- sample(setdiff(list_movie_ids, saved_movie_ids))

if (length(movie_ids_to_save) > 0) {
  saved_series_ids <- series_coll$distinct("id")

  not_in_metacritic <- tibble()

  custom_title_map <- list(
    "Alien³" = "Alien 3",
    "Interview with the Vampire" = "Interview with the Vampire: The Vampire Chronicles",
    "Harry Potter and the Philosopher's Stone" = "Harry Potter and the Sorcerer's Stone",
    "Alien Resurrection" = "Alien: Resurrection",
    "The Good, the Bad and the Ugly" = "The Good, the Bad and the Ugly (re-release)",
    "The French Connection" = "The French Connection (re-release)",
    "Lawrence of Arabia" = "Lawrence of Arabia (re-release)",
    "Mulholland Drive" = "Mulholland Dr.",
    "Birdman" = "Birdman or (The Unexpected Virtue of Ignorance)",
    "Se7en" = "Seven",
    "Lara Croft: Tomb Raider – The Cradle of Life" = "Lara Croft Tomb Raider: The Cradle of Life",
    "Harry Potter and the Deathly Hallows: Part 1" = "Harry Potter and the Deathly Hallows: Part I",
    "Men in Black 3" = "Men in Black III",
    "28 Days Later" = "28 Days Later...",
    "Automata" = "Autómata",
    "Aliens vs Predator: Requiem" = "AVPR: Aliens vs Predator - Requiem",
    "X2" = "X2: X-Men United",
    "Star Wars" = "Star Wars: Episode IV - A New Hope",
    "Return of the Jedi" = "Star Wars: Episode VI - Return of the Jedi",
    "The Empire Strikes Back" = "Star Wars: Episode V - The Empire Strikes Back",
    "Star Wars: The Force Awakens" = "Star Wars: Episode VII - The Force Awakens",
    "Star Wars: The Last Jedi" = "Star Wars: Episode VIII - The Last Jedi"
  )

  for (id in movie_ids_to_save) {
    movie <- tmdb_get(glue("movie/{id}"))
    message(movie$title)
    movie_df <- as_tibble(compact(
      movie[c("id", "title", "tagline", "overview", "backdrop_path", "poster_path", "runtime", "imdb_id", "release_date")]
    ))

    if (length(movie$belongs_to_collection) > 0) {
      message("   (", movie$belongs_to_collection$name, ")")

      if (!(movie$belongs_to_collection$id %in% saved_series_ids)) {
        series_df <- as_tibble(compact(movie$belongs_to_collection))
        res <- series_coll$insert(series_df)
        if (res$nInserted != 1) stop("Didn't insert series")
        saved_series_ids <- c(saved_series_ids, movie$belongs_to_collection$id)
      }

      movie_df$series_id <- movie$belongs_to_collection$id
    }

    genres <- as_tibble(movie$genres)$name
    movie_df$genres <- list(genres)

    credits <- tmdb_get(glue("movie/{id}/credits"))

    actors <- as_tibble(credits$cast)
    movie_df$actors <- list(actors$name)

    directors <- as_tibble(credits$crew) %>% filter(job == "Director")
    movie_df$directors <- list(directors$name)

    # metacritic
    if (movie$title %in% names(custom_title_map)) {
      search_title <- custom_title_map[[movie$title]]
    } else {
      search_title <- movie$title
    }

    title_escaped <- curl_escape(str_replace_all(search_title, "\\/", " "))
    url <- glue("https://www.metacritic.com/search/movie/{title_escaped}/results")
    mc_html <- read_html(url)

    results_titles <- mc_html %>%
      html_nodes("ul.search_results h3.product_title a") %>%
      html_text() %>%
      str_trim()

    result_ix <- which(tolower(results_titles) == tolower(search_title))

    if (length(result_ix) > 1) {
      years <- mc_html %>%
        html_nodes("ul.search_results h3.product_title + p") %>%
        html_text() %>%
        str_trim() %>%
        str_replace("^Movie, ", "")

      result_ix <- which(
        (tolower(results_titles) == tolower(search_title)) &
          (years == str_sub(movie$release_date, 1, 4))
      )
    }

    if (length(result_ix) == 1) {
      metacritic_scores <- mc_html %>%
        html_nodes("ul.search_results span.metascore_w") %>%
        html_text() %>%
        as.integer()

      movie_df$metacritic_score <- metacritic_scores[result_ix]
    } else {
      message("...not in metacritic")
      not_in_metacritic <- bind_rows(not_in_metacritic, movie_df)
    }

    res <- movies_coll$insert(movie_df)
    if (res$nInserted != 1) stop("Didn't insert movie")
  }
}

movies <- as_tibble(movies_coll$find()) %>%
  mutate(
    year = as.integer(str_sub(release_date, 1, 4)),
    poster_url = glue("{configuration$images$base_url}w92{poster_path}"),
    hours = runtime %/% 60,
    minutes = runtime %% 60
  ) %>%
  mutate(pretty_runtime = ifelse(is.na(hours), NA, glue("{hours}h {minutes}m")))

series <- as_tibble(series_coll$find())

series_movie_counts <- movies %>%
  filter(!is.na(series_id)) %>%
  count(series_id)

series_genre_counts <- movies %>%
  filter(!is.na(series_id)) %>%
  select(series_id, genre = genres) %>%
  unnest(genre) %>%
  count(series_id, genre)

series_genres <- inner_join(
  series_movie_counts,
  series_genre_counts,
  by = "series_id"
) %>%
  filter(n.y >= n.x * 2 / 3) %>%
  select(series_id, genre) %>%
  group_by(series_id) %>%
  summarize(genres = list(genre))

movies_final <- movies %>%
  left_join(series_genres, by = "series_id") %>%
  rowwise() %>%
  mutate(
    actors = list(actors[seq(1, min(length(actors), 8))]),
    genres = ifelse(is.null(genres.y), list(genres.x), list(genres.y))
  ) %>%
  ungroup() %>%
  mutate(
    sort_title = rank(gsub("^(The|A|An) (.*)$", '\\2, \\1', title))
  ) %>%
  arrange(sort_title) %>%
  select(
    id, title, sort_title, overview, runtime, imdb_id, actors, directors,
    metacritic_score, year, hours, minutes, poster_url, pretty_runtime, genres
  )

genres <- sort(unique(unlist(movies$genres)))

rm(movies_coll, series_coll)
gc()
