app_server <- function(input, output, session) {
  brand <- shiny::reactive(brand_state(input))
  brand_slow <- shiny::debounce(brand, millis = 600)
  dashboard_server("tips", brand_slow)
  shiny::observeEvent(
    brand_slow(),
    session$setCurrentTheme(brand_theme(brand_slow())),
    ignoreInit = TRUE
  )
  preview_server("preview", brand)
  shiny::observeEvent(input$reset, {
    shiny::updateSelectInput(session, "bootswatch", selected = "flatly")
    session$sendCustomMessage(
      "reset-colors",
      list(
        background = "#FFFFFF",
        foreground = "#212529",
        primary = "#2C6E9B",
        secondary = "#6C757D"
      )
    )
  })
  output$download_brand <- shiny::downloadHandler("_brand.yml", function(file) {
    writeLines(brand_yaml(brand()), file)
  })
  output$download_kit <- shiny::downloadHandler(
    function() paste0("brand-starter-", brand()$bootswatch, ".zip"),
    function(file) create_kit(brand(), file)
  )
}
