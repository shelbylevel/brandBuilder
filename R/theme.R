system_fonts <- c("Arial", "monospace", "sans-serif", "serif")

# Google font -> bslib font (linked, not downloaded); system font -> as-is
font_arg <- function(f) {
  if (f %in% system_fonts) f else bslib::font_google(f, local = FALSE)
}

# bslib theme built from the app's current brand state
brand_theme <- function(x) {
  args <- list(
    version = 5,
    bg = x$background,
    fg = x$foreground,
    primary = x$primary,
    secondary = x$secondary,
    base_font = font_arg(x$base),
    heading_font = font_arg(x$headings),
    code_font = font_arg(x$mono),
    "headings-font-weight" = 600
  )
  if (!identical(x$bootswatch, "default")) {
    args$bootswatch <- x$bootswatch
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
