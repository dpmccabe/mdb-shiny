ui <- function(request) {
  htmlTemplate(
    text_ = as.character(readLines("index.html")),

    title_filter = uiOutput("title_filter"),
    genre_filter = uiOutput("genre_filter"),
    sort_by = uiOutput("sort_by"),

    movies = uiOutput("movies")
  )
}
