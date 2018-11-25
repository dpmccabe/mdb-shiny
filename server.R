server <- function(input, output, session) {
  movie_to_card <- function(movie) {
  	if (str_length(movie$title) < 50) {
			movie_title <- h5(class = "card-title", movie$title)
  	} else {
			movie_title <- h6(class = "card-title", movie$title)
  	}

    if (!is.na(movie$pretty_runtime)) {
      runtime <- list(
        HTML("&nbsp;·&nbsp;"),
        span(class = "runtime", movie$pretty_runtime)
      )
    } else {
      runtime <- NULL
    }

    if (!is.na(movie$metacritic_score)) {
      rating <- div(
        class = "rating",
        div(class = "empty"),
        div(class = "filled", style = glue("width: {movie$metacritic_score}%"))
      )
    } else {
      rating <- NULL
    }

    if (length(movie$genres[[1]]) > 0) {
      genres <- str_c(movie$genres[[1]], collapse = ", ")
    } else {
      genres <- HTML("&nbsp;")
    }

    if (length(movie$directors[[1]]) > 0) {
      directors <- str_c(movie$directors[[1]], collapse = ", ")
      directors <- glue("<h6>Director</h6><p>{directors}</p>")
    } else {
      directors <- NULL
    }

    if (length(movie$actors[[1]]) > 0) {
      actors <- movie$actors[[1]]
      cast <- str_c(actors, collapse = ", ")
      cast <- glue("<h6>Cast</h6><p>{cast}</p>")
    } else {
      cast <- NULL
    }

    if (str_length(movie$overview) > 0) {
      if (str_length(movie$imdb_id) > 0) {
        overview <- glue("
          <p>{movie$overview} (<a href='https://www.imdb.com/title/{movie$imdb_id}'>IMDb</a>)</p>
        ")
      } else {
        overview <- p(movie$overview)
      }
    } else {
      overview <- NULL
    }

    popover_html <- glue("
    	{overview} {directors} {cast}
    ") %>% str_replace_all('"', "&quot;")

    div(
      class = "card-container shown",
      `data-title` = movie$title,
      `data-genre` = genres,
      `data-sort_title` = movie$sort_title,
      `data-year` = movie$year,
      `data-runtime` = movie$runtime,
      `data-rating` = 100 - movie$metacritic_score,

      span(
      	class = "badge badge-light",
      	`data-id` = movie$id,
      	"+"
      ),

      a(
        class = "card",
        style = glue("background-image: url('{movie$poster_url}')"),
	      `data-toggle` = "popover",
        role = "button",
        tabindex = 0,
	      `data-content` = HTML(popover_html),

        div(
          class = "card-body",

          div(
            class = "card-text",

            div(
              class = "card-text-inner",
              movie_title,

              div(
                class = "year-runtime",
                span(class = "year", movie$year),
                runtime
              ),
              rating
            ),

            p(
              class = "genres",
              tags$small(genres)
            )
          )
        )
      )
    )
  }

  output$movies <- renderUI({
    movies_filt <- movies_final

    list(
    	movies_filt %>%
    	  select(
    	    id, title, sort_title, overview, imdb_id, actors, directors, metacritic_score, year,
    	    hours, minutes, poster_url, pretty_runtime, genres, runtime
  	    ) %>%
	      split(1:nrow(.)) %>%
	      map(movie_to_card),
	    tags$script("init_popovers();")
	  )
  })
}
