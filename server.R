server <- function(input, output, session) {
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

  sort_by <- reactiveVal("title")
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

  movie_to_card <- function(movie) {
    if (!is.na(movie$runtime)) {
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
      cast <- str_c(actors[seq(1, min(length(actors), 8))], collapse = ", ")
      cast <- glue("<h6>Cast</h6><p>{cast}</p>")
    } else {
      cast <- NULL
    }

    if (str_length(movie$overview) > 0) {
      overview <- p(movie$overview)
    } else {
      overview <- NULL
    }

    popover_html <- glue("
      	<button class='close' type='button' data-dismiss='popover'><span>×</span></button>
      	{overview} {directors} {cast}
    ") %>% str_replace_all('"', "&quot;")

    div(
      class = "card-container",

      a(
        class = "card",
        style = glue("background-image: url('{movie$poster_url}')"),
	      `data-toggle` = "popover",
	      `data-trigger` = "click",
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

    set.seed(1)
    # movies_filt <- movies
    movies_filt <- movies %>% sample_n(10)

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

    movies_filt <- arrange_(movies_filt, sort_by())

    shiny::validate(
      need(
        nrow(movies_filt) > 0,
        "No results."
      ),
      errorClass = "warning"
    )

    list(
    	movies_filt %>%
	      split(1:nrow(.)) %>%
	      map(movie_to_card),
	    tags$script("init_popovers();")
	  )
  })
}
