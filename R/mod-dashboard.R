load_tips <- function() {
  path <- system.file("quarto-report/data/tips.csv", package = "brandBuilder")
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  d$day <- factor(d$day, levels = c("Thur", "Fri", "Sat", "Sun"))
  d$time <- factor(d$time, levels = c("Lunch", "Dinner"))
  d$tip_pct <- d$tip / d$total_bill
  d
}

# Copy of the first ("Dashboard") tab of the bslib themer demo
dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  tips <- load_tips()

  gear_header <- function(title, ...) {
    bslib::card_header(
      title,
      bslib::popover(
        bsicons::bs_icon("gear"),
        ...,
        title = "Chart options",
        placement = "top"
      ),
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
      shiny::sliderInput(
        ns("total_bill"),
        "Bill amount",
        min(tips$total_bill),
        max(tips$total_bill),
        value = range(tips$total_bill),
        pre = "$"
      ),
      shiny::checkboxGroupInput(
        ns("time"),
        "Food service",
        c("Lunch", "Dinner"),
        c("Lunch", "Dinner"),
        inline = TRUE
      ),
      shiny::actionButton(ns("reset"), "Reset filter")
    ),
    bslib::layout_column_wrap(
      width = 1 / 3,
      fill = FALSE,
      bslib::value_box(
        "Total tippers",
        shiny::uiOutput(ns("total_tippers"), container = htmltools::h2),
        showcase = bsicons::bs_icon("person")
      ),
      bslib::value_box(
        "Average tip",
        shiny::uiOutput(ns("average_tip"), container = htmltools::h2),
        showcase = bsicons::bs_icon("wallet2")
      ),
      bslib::value_box(
        "Average bill",
        shiny::uiOutput(ns("average_bill"), container = htmltools::h2),
        showcase = bsicons::bs_icon("currency-dollar")
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      class = "mt-3",
      bslib::card(
        full_screen = TRUE,
        gear_header(
          "Total bill vs tip",
          shiny::radioButtons(
            ns("scatter_color"),
            "Color by:",
            c("none", "sex", "smoker", "day", "time"),
            inline = TRUE
          )
        ),
        echarts4r::echarts4rOutput(ns("scatter"), height = "320px")
      ),
      bslib::card(
        full_screen = TRUE,
        class = "bslib-card-table-sm",
        bslib::card_header("Tips data"),
        DT::dataTableOutput(ns("table"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      class = "mt-3",
      gear_header(
        "Tip percentages",
        shiny::radioButtons(
          ns("tip_perc_by"),
          "Split by:",
          c("sex", "smoker", "day", "time"),
          selected = "day",
          inline = TRUE
        )
      ),
      echarts4r::echarts4rOutput(ns("tip_perc"), height = "320px")
    )
  )
}

# `brand` is a reactive returning the brand state list
dashboard_server <- function(id, brand) {
  shiny::moduleServer(id, function(input, output, session) {
    tips <- load_tips()

    filtered <- shiny::reactive({
      d <- tips
      d <- d[
        d$total_bill >= input$total_bill[1] &
          d$total_bill <= input$total_bill[2],
      ]
      d[d$time %in% input$time, ]
    })

    output$total_tippers <- shiny::renderUI(nrow(filtered()))
    output$average_bill <- shiny::renderUI({
      d <- filtered()
      if (nrow(d) == 0) return(htmltools::HTML("&ndash;"))
      scales::dollar(mean(d$total_bill))
    })
    output$average_tip <- shiny::renderUI({
      d <- filtered()
      if (nrow(d) == 0) return(htmltools::HTML("&ndash;"))
      scales::percent(mean(d$tip_pct), accuracy = 0.1)
    })

    output$table <- DT::renderDataTable({
      d <- filtered()[, c("total_bill", "tip", "sex", "smoker", "day", "time", "size")]
      DT::datatable(
        d,
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tp", scrollX = TRUE)
      )
    })

    output$scatter <- echarts4r::renderEcharts4r({
      x <- brand()
      d <- filtered()
      shiny::validate(shiny::need(
        nrow(d) > 0,
        "No tips match the current filter. Try adjusting your filter settings."
      ))
      col <- input$scatter_color

      if (identical(col, "none")) {
        d <- d[order(d$total_bill), ]
        e <- echarts4r::e_charts(d, total_bill) |>
          echarts4r::e_scatter(tip, name = "Tips", symbol_size = 7)
        if (nrow(d) >= 15) {
          e <- e |>
            echarts4r::e_loess(tip ~ total_bill, smooth = TRUE, name = "Trend")
        }
        n <- 2
        legend <- FALSE
      } else {
        d <- dplyr::group_by(d, dplyr::across(dplyr::all_of(col)))
        e <- echarts4r::e_charts(d, total_bill) |>
          echarts4r::e_scatter(tip, symbol_size = 7)
        n <- length(unique(d[[col]]))
        legend <- TRUE
      }

      e |>
        echarts_brand(x, n = n, legend = legend) |>
        echarts4r::e_x_axis(
          type = "value",
          axisLabel = list(
            color = x$foreground,
            formatter = htmlwidgets::JS("function(v){return '$' + v;}")
          )
        ) |>
        echarts4r::e_y_axis(
          axisLabel = list(
            color = x$foreground,
            formatter = htmlwidgets::JS("function(v){return '$' + v;}")
          )
        )
    })

    output$tip_perc <- echarts4r::renderEcharts4r({
      x <- brand()
      d <- filtered()
      shiny::validate(shiny::need(
        nrow(d) > 0,
        "No tips match the current filter. Try adjusting your filter settings."
      ))
      by <- input$tip_perc_by

      grid <- seq(0, max(d$tip_pct) * 1.1, length.out = 128)
      groups <- split(d$tip_pct, droplevels(factor(d[[by]])))
      dens <- lapply(groups, function(v) {
        if (length(v) < 2) {
          rep(0, length(grid))
        } else {
          stats::density(v, from = min(grid), to = max(grid), n = length(grid))$y
        }
      })
      wide <- data.frame(x = grid, dens, check.names = FALSE)

      e <- echarts4r::e_charts(wide, x)
      for (nm in names(dens)) {
        e <- echarts4r::e_line_(
          e,
          nm,
          name = nm,
          smooth = TRUE,
          symbol = "none",
          areaStyle = list(opacity = 0.35)
        )
      }

      e |>
        echarts_brand(x, n = length(dens), legend = TRUE) |>
        echarts4r::e_x_axis(
          type = "value",
          axisLabel = list(
            color = x$foreground,
            formatter = htmlwidgets::JS(
              "function(v){return (v * 100).toFixed(0) + '%';}"
            )
          )
        )
    })

    shiny::observeEvent(input$reset, {
      shiny::updateSliderInput(
        session,
        "total_bill",
        value = range(tips$total_bill)
      )
      shiny::updateCheckboxGroupInput(
        session,
        "time",
        selected = c("Lunch", "Dinner")
      )
    })
  })
}
