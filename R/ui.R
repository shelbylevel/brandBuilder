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
  bslib::page_navbar(
    title = "Brand Builder",
    theme = bslib::bs_theme(bootswatch = "litera"),
    header = assets(),
    sidebar = bslib::sidebar(
      width = 350,
      open = "always",
      htmltools::h2("Build your style", class = "h5"),
      shiny::selectInput(
        "bootswatch",
        "Bootswatch foundation",
        choices,
        "flatly"
      ),
      bslib::accordion(
        open = c("Colors", "Typography"),
        bslib::accordion_panel(
          "Colors",
          color_control("background", "Background", "#FFFFFF"),
          color_control("foreground", "Foreground", "#212529"),
          color_control("primary", "Primary", "#2C6E9B"),
          color_control("secondary", "Secondary", "#6C757D")
        ),
        bslib::accordion_panel(
          "Typography",
          shiny::selectInput(
            "base_font",
            "Base font",
            c("Arial", "Figtree", "Inter", "Open Sans")
          ),
          shiny::selectInput(
            "heading_font",
            "Heading font",
            c("Source Sans 3", "Figtree", "Inter")
          ),
          shiny::selectInput(
            "code_font",
            "Code font",
            c("JetBrains Mono", "Fira Code", "monospace")
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
    !!!preview_ui("preview")
  )
}
