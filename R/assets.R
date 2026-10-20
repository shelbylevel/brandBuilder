assets <- function() {
  shiny::addResourcePath("brandbuilder-assets", system.file("assets", package="brandbuilder"))
  htmltools::tagList(
    htmltools::tags$link(rel="stylesheet", href="brandbuilder-assets/app.css"),
    htmltools::tags$script(src="brandbuilder-assets/app.js")
  )
}
