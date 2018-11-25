server <- function(input, output, session) {
  movies <- reactiveVal(movies_final)

  output$title_filter <- renderUI({
    textInput("title_filter", "Title")
  })

  output$genre_filter <- renderUI({
    selectInput(
      "genre_filter",
      label = "Genre",
      choices = c("", genres),
      selectize = F
    )
  })

  sort_by <- reactiveVal("sort_title")
  sort_desc <- reactiveVal(FALSE)

  observeEvent(input$sort_by_trigger, {
    trigger_val <- input$sort_by_trigger$val

    if (sort_by() == "desc(metacritic_score)" && trigger_val == "metacritic_score") {
      sort_desc(F)
    } else if (sort_by() != "metacritic_score" && trigger_val == "metacritic_score") {
      sort_desc(T)
    } else if (trigger_val == sort_by()) {
      sort_desc(T)
    } else {
      sort_desc(F)
    }

    if (sort_desc()) {
      sort_by(as.character(glue("desc({trigger_val})")))
    } else {
      sort_by(trigger_val)
    }
  })

  is_selected <- reactiveVal(NA)
  selected_ids <- reactiveVal(integer(0))

  observeEvent(input$is_selected_trigger, {
    trigger_val <- input$is_selected_trigger$val

    if (trigger_val == "all") {
    	is_selected(NA)
    } else if (trigger_val == "yes") {
    	is_selected(TRUE)
    } else {
    	is_selected(FALSE)
    }
  })

  observeEvent(input$toggle_selected, {
  	movies_to_update <- movies()
  	ix <- which(movies_to_update$id == input$toggle_selected$val)
  	movies_to_update[ix, "selected"] <- !movies_to_update[ix, "selected"]
  	movies(movies_to_update)
  })

  movie_to_card <- function(movie) {
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

    badge_classes <- c("badge", "badge-light")
    if (movie$selected) badge_classes <- c(badge_classes, "selected")

    div(
      class = "card-container",
      span(
      	class = str_c(badge_classes, collapse = " "),
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
              h5(class = "card-title", movie$title),

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
    req(sort_by(), !is.null(input$title_filter), !is.null(input$genre_filter))

    movies_filt <- movies()

    if (str_length(input$genre_filter) > 0) {
      movies_filt <- movies_filt %>%
        rowwise() %>%
        filter(input$genre_filter %in% genres) %>%
        ungroup()
    }

    if (str_length(input$title_filter) > 0) {
      movies_filt <- movies_filt %>%
        filter(str_detect(title, coll(input$title_filter, ignore_case = T)))
    }

    if (!is.na(is_selected())) {
      if (is_selected()) {
        movies_filt <- filter(movies_filt, selected)
      } else if (!is_selected()) {
        movies_filt <- filter(movies_filt, !selected)
      }
    }

    shiny::validate(
      need(
        nrow(movies_filt) > 0,
        "No results."
      ),
      errorClass = "warning"
    )

    movies_filt <- movies_filt %>%
    	mutate(random = runif(1:n())) %>%
    	arrange_(sort_by())

    list(
    	movies_filt %>%
    	  select(
    	    id, title, overview, imdb_id, actors, directors, metacritic_score, year,
    	    hours, minutes, poster_url, pretty_runtime, genres, selected
  	    ) %>%
	      split(1:nrow(.)) %>%
	      map(movie_to_card),
	    tags$script("init_popovers();")
	  )
  })
}
