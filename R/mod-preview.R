preview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    bslib::nav_panel("Shiny preview", shiny::uiOutput(ns("shiny"))),
    bslib::nav_panel("Quarto preview", shiny::uiOutput(ns("quarto"))),
    bslib::nav_panel(
      "Brand YAML",
      bslib::card(
        bslib::card_header("Generated _brand.yml"),
        htmltools::tags$pre(shiny::textOutput(ns("yaml"))),
        bslib::card_footer(shiny::actionButton(ns("copy"), "Copy YAML"))
      )
    ),
    bslib::nav_panel("Accessibility", shiny::uiOutput(ns("a11y")))
  )
}
preview_server <- function(id, brand) {
  shiny::moduleServer(id, function(input, output, session) {
    output$shiny <- shiny::renderUI(htmltools::tags$iframe(
      class = "preview-frame",
      title = "Shiny preview",
      srcdoc = htmltools::HTML(preview_doc(brand(), "shiny"))
    ))
    output$quarto <- shiny::renderUI(htmltools::tags$iframe(
      class = "preview-frame",
      title = "Quarto preview",
      srcdoc = htmltools::HTML(preview_doc(brand(), "quarto"))
    ))
    output$yaml <- shiny::renderText(brand_yaml(brand()))
    output$a11y <- shiny::renderUI({
      x <- brand()
      vals <- list(
        c("Body text/background", contrast(x$foreground, x$background), 4.5),
        c("White/primary", contrast("#FFFFFF", x$primary), 4.5),
        c("Primary/background", contrast(x$primary, x$background), 3)
      )
      bslib::layout_column_wrap(
        width = 1 / 3,
        lapply(vals, function(v) {
          bslib::card(
            bslib::card_header(v[1]),
            htmltools::p(
              class = "display-6",
              sprintf("%.2f:1", as.numeric(v[2]))
            ),
            htmltools::p(
              if (as.numeric(v[2]) >= as.numeric(v[3])) "Pass" else "Review"
            )
          )
        })
      )
    })
    shiny::observeEvent(input$copy, {
      session$sendCustomMessage("copy-yaml", brand_yaml(brand()))
    })
  })
}
