preview_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
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
    # --- Shiny ----------------------------------------------------------
    output$shiny <- shiny::renderUI(htmltools::tags$iframe(
      class = "preview-frame",
      title = "Shiny preview",
      srcdoc = htmltools::HTML(preview_doc(brand(), "shiny"))
    ))

    # --- Quarto report: scratch dir, one per session ---------------------
    work_dir <- tempfile("brand-preview-")
    dir.create(work_dir)
    work_dir <- normalizePath(work_dir)

    # copy the whole template folder (index.qmd, data/tips.csv, ...)
    template_dir <- system.file("quarto-report", package = "brandBuilder")
    stopifnot(nzchar(template_dir))
    file.copy(
      list.files(template_dir, full.names = TRUE),
      work_dir,
      recursive = TRUE
    )

    session$onSessionEnded(function() unlink(work_dir, recursive = TRUE))

    # wait for the user to pause before re-rendering
    brand_slow <- shiny::debounce(brand, millis = 800)

    prefix <- paste0("brand-preview-", session$token)
    shiny::addResourcePath(prefix, work_dir)
    render_n <- 0

    report_url <- shiny::reactive({
      x <- brand_slow()
      swatch <- x$bootswatch %||% "flatly"

      writeLines(brand_yaml(x), file.path(work_dir, "_brand.yml"))
      writeLines(
        c("format:", "  html:", sprintf("    theme: [%s, brand]", swatch)),
        file.path(work_dir, "_quarto.yml")
      )

      out <- file.path(work_dir, "index.html")
      unlink(out)

      shiny::withProgress(message = "Rendering report...", {
        tryCatch(
          quarto::quarto_render(
            file.path(work_dir, "index.qmd"),
            quiet = FALSE
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Report render failed:", conditionMessage(e)),
              type = "error"
            )
          }
        )
      })

      shiny::req(file.exists(out))
      render_n <<- render_n + 1
      sprintf("%s/index.html?v=%d", prefix, render_n)
    })

    output$quarto <- shiny::renderUI(
      htmltools::tags$iframe(
        class = "preview-frame",
        title = "Quarto preview",
        src = report_url()
      )
    )
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
