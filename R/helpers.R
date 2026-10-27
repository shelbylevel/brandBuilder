`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
brand_state <- function(input) {
  list(
    bootswatch = input$bootswatch %||% "flatly",
    background = input$background_value %||% "#FFFFFF",
    foreground = input$foreground_value %||% "#212529",
    primary = input$primary_value %||% "#2C6E9B",
    secondary = input$secondary_value %||% "#6C757D",
    base = input$base_font %||% "Figtree",
    headings = input$heading_font %||% "Source Sans 3",
    mono = input$code_font %||% "JetBrains Mono"
  )
}
brand_yaml <- function(x) {
  yaml::as.yaml(list(
    meta = list(name = "My brand"),
    color = list(
      background = x$background,
      foreground = x$foreground,
      primary = x$primary,
      secondary = x$secondary
    ),
    typography = list(
      fonts = list(
        list(family = x$base, source = "google"),
        list(family = x$headings, source = "google"),
        list(family = x$mono, source = "google")
      ),
      base = x$base,
      headings = list(family = x$headings, weight = 600),
      monospace = x$mono
    )
  ))
}
luminance <- function(hex) {
  v <- grDevices::col2rgb(hex) / 255
  v <- ifelse(v <= .04045, v / 12.92, ((v + .055) / 1.055)^2.4)
  sum(v * c(.2126, .7152, .0722))
}
contrast <- function(a, b) {
  v <- sort(c(luminance(a), luminance(b)), decreasing = TRUE)
  (v[1] + .05) / (v[2] + .05)
}
# preview_doc <- function(x, kind = c("shiny", "quarto")) {
#   kind <- match.arg(kind)
#   css <- if (x$bootswatch == "default") {
#     "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
#   } else {
#     sprintf(
#       "https://cdn.jsdelivr.net/npm/bootswatch@5.3.3/dist/%s/bootstrap.min.css",
#       x$bootswatch
#     )
#   }
#   body <- if (kind == "shiny") {
#     '<nav class="navbar bg-primary" data-bs-theme="dark"><div class="container-fluid"><span class="navbar-brand">Community Health Dashboard</span></div></nav><main class="container py-4"><div class="row g-4"><aside class="col-md-4"><div class="card"><div class="card-body"><h2 class="h5">Filters</h2><label class="form-label">Region</label><select class="form-select mb-3"><option>All regions</option></select><button class="btn btn-primary">Apply filters</button></div></div></aside><section class="col-md-8"><h1>Overview</h1><p>Explore health measures and trends.</p><div class="card"><div class="card-header"><h2 class="h5 mb-0">Current estimate</h2></div><div class="card-body"><p class="display-4">72.4%</p><div class="progress"><div class="progress-bar" style="width:72%">72%</div></div></div></div></section></div></main>'
#   } else {
#     '<main class="container py-5" style="max-width:850px"><article><header><h1 class="display-4">Community Health Report</h1><p class="lead">A Quarto-style report preview.</p></header><h2>Overview</h2><p>This shows body text, <a href="#">links</a>, headings, tables, callouts, and code.</p><div class="alert alert-primary"><h3 class="h5">Key finding</h3><p class="mb-0">A consistent visual system supports the content.</p></div><h2>Results</h2><table class="table table-striped"><thead><tr><th>Region</th><th>Estimate</th></tr></thead><tbody><tr><td>Central</td><td>72.4%</td></tr><tr><td>Northeast</td><td>68.1%</td></tr></tbody></table><pre class="p-3 bg-body-tertiary"><code>ggplot(results, aes(period, estimate))</code></pre></article></main>'
#   }
#   sprintf(
#     '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width"><link rel="stylesheet" href="%s"><style>:root{--p:%s;--s:%s}body{background:%s;color:%s;font-family:"%s",sans-serif}h1,h2,h3{font-family:"%s",sans-serif}.btn-primary,.progress-bar{--bs-btn-bg:var(--p);--bs-btn-border-color:var(--p);background-color:var(--p)}a{color:var(--p)}code{font-family:"%s",monospace}</style></head><body>%s</body></html>',
#     css,
#     x$primary,
#     x$secondary,
#     x$background,
#     x$foreground,
#     x$base,
#     x$headings,
#     x$mono,
#     body
#   )
# }
create_kit <- function(x, file) {
  d <- tempfile()
  dir.create(d)
  dir.create(file.path(d, "shiny"))
  dir.create(file.path(d, "quarto"))
  writeLines(brand_yaml(x), file.path(d, "_brand.yml"))
  writeLines(
    sprintf(
      'library(shiny)\nlibrary(bslib)\nui <- page_sidebar(title="Brand starter", theme=bs_theme(bootswatch="%s"), sidebar=sidebar("Controls"), card("Content"))\nserver <- function(input,output,session){}\nshinyApp(ui,server)',
      x$bootswatch
    ),
    file.path(d, "shiny", "app.R")
  )
  writeLines(
    '---\ntitle: "Brand report"\nformat: html\n---\n\n## Overview\n\nStarter report.',
    file.path(d, "quarto", "report.qmd")
  )
  o <- setwd(d)
  on.exit(setwd(o))
  utils::zip(file, list.files(".", recursive = TRUE))
}
