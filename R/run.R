#' Run the Brand Builder
#' @param ... Passed to [shiny::runApp()].
#' @export
run_app <- function(...) shiny::runApp(shiny::shinyApp(app_ui, app_server), ...)
