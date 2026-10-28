choices <- c(
  "Default" = "default",
  "Cerulean" = "cerulean",
  "Cosmo" = "cosmo",
  "Cyborg" = "cyborg",
  "Darkly" = "darkly",
  "Flatly" = "flatly",
  "Journal" = "journal",
  "Litera" = "litera",
  "Lumen" = "lumen",
  "Lux" = "lux",
  "Materia" = "materia",
  "Minty" = "minty",
  "Morph" = "morph",
  "Pulse" = "pulse",
  "Quartz" = "quartz",
  "Sandstone" = "sandstone",
  "Simplex" = "simplex",
  "Sketchy" = "sketchy",
  "Slate" = "slate",
  "Solar" = "solar",
  "Spacelab" = "spacelab",
  "Superhero" = "superhero",
  "United" = "united",
  "Vapor" = "vapor",
  "Yeti" = "yeti",
  "Zephyr" = "zephyr"
)
color_control <- function(id, label, value) {
  htmltools::div(
    class = "color-control",
    htmltools::tags$label(`for` = id, label),
    htmltools::div(
      class = "color-row",
      htmltools::tags$input(id = id, type = "color", value = value),
      htmltools::tags$input(
        id = paste0(id, "_text"),
        value = value,
        class = "form-control"
      )
    )
  )
}
app_ui <- function(request) {
  # Controls start at the default Bootswatch's own values; picking another
  # Bootswatch updates them (see app_server()).
  d <- bootswatch_defaults(DEFAULT_BOOTSWATCH)

  bslib::page_navbar(
    title = "Brand Builder",
    theme = brand_theme(brand_state(list())),
    header = assets(),
    sidebar = bslib::sidebar(
      width = 350,
      open = "always",
      htmltools::h2("Build your style", class = "h5"),
      shiny::selectInput(
        "bootswatch",
        "Bootswatch foundation",
        choices,
        DEFAULT_BOOTSWATCH
      ),
      bslib::accordion(
        open = c("Colors", "Typography"),
        bslib::accordion_panel(
          "Colors",
          color_control("background", "Background", d$background),
          color_control("foreground", "Foreground", d$foreground),
          color_control("primary", "Primary", d$primary),
          color_control("secondary", "Secondary", d$secondary)
        ),
        bslib::accordion_panel(
          "Typography",
          shiny::selectInput(
            "base_font",
            "Base font",
            font_choices(base_fonts, d$base),
            d$base
          ),
          shiny::selectInput(
            "heading_font",
            "Heading font",
            font_choices(heading_fonts, d$headings),
            d$headings
          ),
          shiny::selectInput(
            "code_font",
            "Code font",
            font_choices(code_fonts, d$mono),
            d$mono
          )
        )
      ),
      htmltools::div(
        class = "d-grid gap-2 mt-3",
        shiny::actionButton("reset", "Reset"),
        shiny::downloadButton("download_brand", "Download _brand.yml"),
        shiny::downloadButton("download_kit", "Download starter kit")
      )
    ),
    bslib::nav_panel("Dashboard", dashboard_ui("tips")),
    !!!preview_ui("preview")
  )
}
