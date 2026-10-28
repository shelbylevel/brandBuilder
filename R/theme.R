system_fonts <- c(
  "Arial",
  "Helvetica",
  "Helvetica Neue",
  "Segoe UI",
  "system-ui",
  "SFMono-Regular",
  "Menlo",
  "Monaco",
  "Consolas",
  "Courier New",
  "monospace",
  "sans-serif",
  "serif"
)

is_system_font <- function(f) f %in% system_fonts

# brand.yml `source` for a font
font_source <- function(f) if (is_system_font(f)) "system" else "google"

# Google font -> bslib font (linked, not downloaded); system font -> as-is
font_arg <- function(f) {
  if (is_system_font(f)) f else bslib::font_google(f, local = FALSE)
}

# bslib theme built from the app's current brand state.
#
# Like bs_themer(), only values that differ from the selected Bootswatch's own
# are passed to bs_theme(). Untouched controls leave the Bootswatch exactly as
# its author designed it, and edits are layered on top.
brand_theme <- function(x) {
  d <- bootswatch_defaults(x$bootswatch)
  same_color <- function(a, b) identical(toupper(a), toupper(b))

  args <- list(version = 5, brand = FALSE, "headings-font-weight" = 600)
  if (!identical(x$bootswatch, "default")) {
    args$bootswatch <- x$bootswatch
  }

  # bs_theme() needs bg and fg together when either one changes
  if (
    !same_color(x$background, d$background) ||
      !same_color(x$foreground, d$foreground)
  ) {
    args$bg <- x$background
    args$fg <- x$foreground
  }
  if (!same_color(x$primary, d$primary)) {
    args$primary <- x$primary
  }
  if (!same_color(x$secondary, d$secondary)) {
    args$secondary <- x$secondary
  }

  base_changed <- !identical(x$base, d$base)
  if (base_changed) {
    args$base_font <- font_arg(x$base)
  }
  # Many Bootswatches leave the heading font unset, so headings follow the base
  # font. Pin it to what the Heading font control shows once either changes.
  if (base_changed || !identical(x$headings, d$headings)) {
    args$heading_font <- font_arg(x$headings)
  }
  if (!identical(x$mono, d$mono)) {
    args$code_font <- font_arg(x$mono)
  }

  do.call(bslib::bs_theme, args)
}

rgba <- function(hex, alpha) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

# n colors running from primary to secondary
brand_palette <- function(x, n) {
  if (n <= 1) {
    return(x$primary)
  }
  grDevices::colorRampPalette(c(x$primary, x$secondary))(n)
}

# Apply the brand to any echarts4r chart
echarts_brand <- function(e, x, n = 1, legend = FALSE) {
  fg <- x$foreground
  grid <- rgba(fg, 0.15)
  e |>
    echarts4r::e_color(brand_palette(x, n), background = "rgba(0,0,0,0)") |>
    echarts4r::e_text_style(color = fg, fontFamily = x$base) |>
    echarts4r::e_x_axis(
      axisLabel = list(color = fg),
      axisLine = list(lineStyle = list(color = grid)),
      splitLine = list(show = FALSE)
    ) |>
    echarts4r::e_y_axis(
      axisLabel = list(color = fg),
      splitLine = list(lineStyle = list(color = grid))
    ) |>
    echarts4r::e_legend(show = legend, textStyle = list(color = fg)) |>
    echarts4r::e_tooltip(trigger = "item")
}
