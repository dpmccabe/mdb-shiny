ui <- function(request) {
  htmlTemplate(
    text_ = as.character(readLines("index.html")),
    movies = uiOutput("movies")
  )
}
