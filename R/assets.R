assets <- function() {
  shiny::addResourcePath(
    "brandBuilder-assets",
    system.file("assets", package = "brandBuilder")
  )
  htmltools::tagList(
    htmltools::tags$link(
      rel = "stylesheet",
      href = "brandBuilder-assets/app.css"
    ),
    htmltools::tags$script(src = "brandBuilder-assets/app.js")
  )
}
