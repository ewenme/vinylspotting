#' Run contained Shiny app in a web browser
#'
#' \code{vinylspotting} provides an interactive tool to explore
#' Discogs collections. The tool will be launched in a web browser.
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "vinylspotting"),
                display.mode = "normal", launch.browser = TRUE)
}
