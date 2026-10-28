app_server <- function(input, output, session) {
  # --- Bootswatch -> sidebar controls ----------------------------------------
  # Choosing a Bootswatch resets Colors and Typography to that Bootswatch's own
  # values (like the bslib themer demo). Edits made afterwards sit on top of
  # them. See brand_theme() for how edits are applied.
  #
  # The values travel to the browser and come back as inputs, so for a moment
  # the inputs still hold the old Bootswatch's values. `sync_sent` counts the
  # syncs sent, and app.js sets `input$controls_synced` once it has applied one.
  # While they differ, brand() uses the Bootswatch's values directly instead of
  # the stale inputs, so the theme never flashes the wrong colors.
  sync_sent <- shiny::reactiveVal(0L)
  pushed_for <- shiny::reactiveVal(DEFAULT_BOOTSWATCH)

  push_defaults <- function(bootswatch) {
    d <- bootswatch_defaults(bootswatch)
    pushed_for(bootswatch)
    id <- shiny::isolate(sync_sent()) + 1L
    sync_sent(id)

    shiny::updateSelectInput(
      session,
      "base_font",
      choices = font_choices(base_fonts, d$base),
      selected = d$base
    )
    shiny::updateSelectInput(
      session,
      "heading_font",
      choices = font_choices(heading_fonts, d$headings),
      selected = d$headings
    )
    shiny::updateSelectInput(
      session,
      "code_font",
      choices = font_choices(code_fonts, d$mono),
      selected = d$mono
    )
    # the color inputs are custom HTML, so app.js sets them
    session$sendCustomMessage(
      "sync-controls",
      list(
        id = id,
        colors = d[c("background", "foreground", "primary", "secondary")]
      )
    )
  }

  # The UI is built with the default Bootswatch's values, so nothing to do at
  # startup.
  shiny::observeEvent(
    input$bootswatch,
    {
      if (!identical(input$bootswatch, shiny::isolate(pushed_for()))) {
        push_defaults(input$bootswatch)
      }
    },
    ignoreInit = TRUE
  )

  brand <- shiny::reactive({
    synced <- as.integer(input$controls_synced %||% 0L)
    if (sync_sent() > synced) {
      brand_defaults_state(pushed_for())
    } else {
      brand_state(input)
    }
  })
  brand_slow <- shiny::debounce(brand, millis = 600)
  dashboard_server("tips", brand_slow)
  shiny::observeEvent(
    brand_slow(),
    session$setCurrentTheme(brand_theme(brand_slow())),
    ignoreInit = TRUE
  )
  preview_server("preview", brand)

  shiny::observeEvent(input$reset, {
    shiny::updateSelectInput(
      session,
      "bootswatch",
      selected = DEFAULT_BOOTSWATCH
    )
    push_defaults(DEFAULT_BOOTSWATCH)
  })

  output$download_brand <- shiny::downloadHandler("_brand.yml", function(file) {
    writeLines(brand_yaml(brand()), file)
  })
  output$download_kit <- shiny::downloadHandler(
    function() paste0("brand-starter-", brand()$bootswatch, ".zip"),
    function(file) create_kit(brand(), file)
  )
}
