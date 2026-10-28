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

# ggplot theme using the brand colors (same idea as the Quarto report)
theme_brand_app <- function(x, base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = x$foreground),
      axis.text = ggplot2::element_text(color = x$foreground),
      plot.background = ggplot2::element_rect(fill = x$background, color = NA),
      panel.background = ggplot2::element_rect(fill = x$background, color = NA),
      panel.grid.major = ggplot2::element_line(
        color = grDevices::adjustcolor(x$foreground, 0.12)
      ),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_img <- function(p, width = 6, height = 3.5, res = 110) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  grDevices::png(f, width = width, height = height, units = "in", res = res)
  print(p)
  grDevices::dev.off()
  htmltools::tags$img(
    src = base64enc::dataURI(file = f, mime = "image/png"),
    alt = "",
    style = "width:100%;height:100%;object-fit:contain;"
  )
}

tips_data <- function() {
  path <- system.file("quarto-report/data/tips.csv", package = "brandBuilder")
  utils::read.csv(path, stringsAsFactors = FALSE)
}

html_table <- function(d) {
  htmltools::tags$div(
    style = "overflow:auto;max-height:100%;",
    htmltools::tags$table(
      class = "table table-sm table-striped mb-0",
      htmltools::tags$thead(htmltools::tags$tr(
        lapply(names(d), htmltools::tags$th)
      )),
      htmltools::tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        htmltools::tags$tr(lapply(d[i, ], htmltools::tags$td))
      }))
    )
  )
}

# Static copy of the first ("Dashboard") tab of the bslib themer demo
dashboard_tab <- function(x) {
  tips <- tips_data()
  tips$tip_pct <- tips$tip / tips$total_bill
  tips$day <- factor(tips$day, levels = c("Thur", "Fri", "Sat", "Sun"))

  scatter <- ggplot2::ggplot(tips, ggplot2::aes(total_bill, tip)) +
    ggplot2::geom_point(color = x$foreground) +
    ggplot2::geom_smooth(
      method = "loess",
      formula = y ~ x,
      color = x$primary,
      fill = x$primary
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_brand_app(x)

  ridges <- if (requireNamespace("ggridges", quietly = TRUE)) {
    ggplot2::ggplot(tips, ggplot2::aes(tip_pct, day)) +
      ggridges::geom_density_ridges(
        scale = 0.9,
        fill = x$primary,
        alpha = 0.6,
        color = x$foreground
      )
  } else {
    ggplot2::ggplot(tips, ggplot2::aes(tip_pct, fill = day)) +
      ggplot2::geom_density(alpha = 0.5)
  }
  ridges <- ridges +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_brand_app(x)

  icon_header <- function(title) {
    bslib::card_header(
      title,
      bsicons::bs_icon("gear"),
      class = "d-flex justify-content-between align-items-center"
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = bslib::tooltip(
        htmltools::span(
          "Restaurant tipping ",
          bsicons::bs_icon("info-circle-fill"),
          class = "sidebar-title"
        ),
        "One waiter recorded information about each tip he received",
        "over a period of a few months working in one restaurant."
      ),
      htmltools::tags$label("Bill amount", class = "form-label"),
      htmltools::tags$input(
        type = "range",
        class = "form-range",
        min = 3,
        max = 51,
        value = 30
      ),
      htmltools::tags$label("Food service", class = "form-label"),
      htmltools::div(
        class = "d-flex gap-3",
        htmltools::div(
          class = "form-check",
          htmltools::tags$input(
            type = "checkbox",
            class = "form-check-input",
            id = "pv-lunch",
            checked = NA
          ),
          htmltools::tags$label(
            "Lunch",
            `for` = "pv-lunch",
            class = "form-check-label"
          )
        ),
        htmltools::div(
          class = "form-check",
          htmltools::tags$input(
            type = "checkbox",
            class = "form-check-input",
            id = "pv-dinner",
            checked = NA
          ),
          htmltools::tags$label(
            "Dinner",
            `for` = "pv-dinner",
            class = "form-check-label"
          )
        )
      ),
      htmltools::tags$button("Reset filter", class = "btn btn-default")
    ),
    bslib::layout_column_wrap(
      width = 1 / 3,
      fill = FALSE,
      bslib::value_box(
        "Total tippers",
        htmltools::h2(nrow(tips)),
        showcase = bsicons::bs_icon("person")
      ),
      bslib::value_box(
        "Average tip",
        htmltools::h2(scales::percent(mean(tips$tip_pct), accuracy = 0.1)),
        showcase = bsicons::bs_icon("wallet2")
      ),
      bslib::value_box(
        "Average bill",
        htmltools::h2(scales::dollar(mean(tips$total_bill))),
        showcase = bsicons::bs_icon("currency-dollar")
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      class = "mt-3",
      bslib::card(
        full_screen = TRUE,
        icon_header("Total bill vs tip"),
        bslib::card_body(plot_img(scatter), padding = 0)
      ),
      bslib::card(
        full_screen = TRUE,
        class = "bslib-card-table-sm",
        bslib::card_header("Tips data"),
        bslib::card_body(
          html_table(utils::head(tips[, 1:7], 15)),
          padding = 0
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      class = "mt-3",
      icon_header("Tip percentages"),
      bslib::card_body(plot_img(ridges, height = 3), padding = 0)
    )
  )
}

# Write the preview page into `dir` (index.html + lib/) and return its path
write_shiny_preview <- function(x, dir) {
  page <- bslib::page_navbar(
    title = "Theme demo",
    theme = brand_theme(x),
    inverse = TRUE,
    fillable = "Dashboard",
    bslib::nav_panel("Dashboard", dashboard_tab(x))
  )
  out <- file.path(dir, "index.html")
  htmltools::save_html(page, out, libdir = "lib")
  out
}
