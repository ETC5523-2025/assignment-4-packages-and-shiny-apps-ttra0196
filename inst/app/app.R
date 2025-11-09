# inst/app/app.R
library(shiny)
library(ggplot2)
library(bslib)
library(DT)

get_pkg_data <- function(name) get(name, envir = asNamespace("aussiefirewx"))

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cosmo"),
  tags$head(
    tags$style(HTML("
      .metric-badge {font-weight:600; padding:2px 8px; border-radius:999px; background:#eef5ff;}
      .card { border-radius: 14px; box-shadow: 0 8px 20px rgba(0,0,0,.06); }
      .value { font-size:1.1rem; font-weight:700; }
      .note { color:#555; }
    "))
  ),
  
  # Title
  card(
    card_header(h3("Fire Weather Explorer")),
    card_body(
      p(class = "note",
        "Explore how Fire Weather Index (FWI) and Monthly Severity Rating (MSR) changed over time in south-eastern Australia. ",
        "Use the controls to filter the period and switch metrics. ",
        "Higher values generally mean more dangerous fire weather."
      )
    )
  ),
  
  # Controls + Outputs
  layout_columns(
    col_widths = c(4, 8),
    
    # Controls (meets: interactivity + field descriptions)
    card(
      card_header("Controls"),
      card_body(
        selectInput(
          "metric", "Metric",
          choices = c("Fire Weather Index" = "fwi", "Monthly Severity Rating" = "msr"),
          selected = "fwi"
        ),
        helpText(tags$span(class="metric-badge","Metric meaning:"),
                 tags$br(),
                 tags$b("FWI:"), " composite index of fire weather danger;",
                 tags$br(),
                 tags$b("MSR:"), " monthly fire danger severity."),
        sliderInput("yr", "Year range", min = 1979, max = 2020, value = c(1990, 2020), step = 1),
        checkboxInput("show_ci", "Show smoothing CI", value = FALSE),
        actionButton("reset", "Reset", class = "btn btn-outline-primary"),
        hr(),
        tags$small(class = "note",
                   "Tip: Use FWI for day-to-day fire weather severity; MSR summarises monthly severity."
        )
      )
    ),
    
    # Outputs (plot + stats + table) with interpretations
    card(
      card_header(textOutput("title")),
      card_body(
        plotOutput("ts_plot", height = 340),
        br(),
        layout_columns(
          col_widths = c(4,4,4),
          card(
            card_header("Trend (linear slope)"),
            card_body(
              div(class="value", textOutput("slope_txt")),
              p(class="note", "Units per year (based on selected period and metric).")
            )
          ),
          card(
            card_header("Change vs start"),
            card_body(
              div(class="value", textOutput("pct_txt")),
              p(class="note", "Percent change from first year to last year in the selected range.")
            )
          ),
          card(
            card_header("How to interpret"),
            card_body(uiOutput("interpretation"))
          )
        ),
        hr(),
        h5("Data table"),
        DTOutput("tbl")
      )
    )
  )
)

server <- function(input, output, session) {
  # Reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "metric", selected = "fwi")
    updateSliderInput(session, "yr", value = c(1990, 2020))
    updateCheckboxInput(session, "show_ci", value = FALSE)
  })
  
  # Use the dataset FROM THE PACKAGE (no read.csv)
  dat_all <- get_pkg_data("se_aus_fwi")
  
  rng <- reactive({
    subset(dat_all, year >= input$yr[1] & year <= input$yr[2])
  })
  
  output$title <- renderText({
    paste0("Trend: ", if (input$metric == "fwi") "FWI" else "MSR",
           " (", input$yr[1], "–", input$yr[2], ")")
  })
  
  # Linear model for slope and p-value
  fit <- reactive({
    m <- input$metric
    lm(rng()[[m]] ~ rng()[["year"]])
  })
  
  slope_val <- reactive({
    round(unname(coef(fit())[2]), 3)
  })
  
  pct_change <- reactive({
    m <- input$metric
    d <- rng()[[m]]
    if (length(d) < 2 || isTRUE(all.equal(d[1], 0))) return(NA_real_)
    round(100 * (tail(d, 1) - d[1]) / abs(d[1]), 1)
  })
  
  output$slope_txt <- renderText({
    s <- slope_val()
    ifelse(is.finite(s), paste0(s, " / year"), "NA")
  })
  
  output$pct_txt <- renderText({
    p <- pct_change()
    ifelse(is.finite(p), paste0(p, "%"), "NA")
  })
  
  # Plot that responds to all controls
  output$ts_plot <- renderPlot({
    m <- input$metric
    ggplot(rng(), aes(year, .data[[m]])) +
      geom_line(linewidth = 1) +
      geom_point(alpha = .6) +
      geom_smooth(method = "lm", se = input$show_ci, linewidth = .8) +
      labs(
        x = "Year",
        y = toupper(m),
        caption = "Illustrative dataset packaged in aussiefirewx"
      )
  })
  
  # Interpretation text
  output$interpretation <- renderUI({
    s <- slope_val()
    p <- summary(fit())$coefficients[2, 4]
    dir <- if (is.finite(s) && s > 0) "upward" else if (is.finite(s) && s < 0) "downward" else "flat"
    span(
      if (!is.finite(s)) {
        "Not enough data in the selected range."
      } else {
        paste0("The ", if (input$metric == "fwi") "FWI" else "MSR",
               " shows a ", dir, " trend. ",
               "Slope ≈ ", format(s, digits = 3),
               " per year; p-value = ", format.pval(p, digits = 3), ". ",
               "Higher values indicate greater fire weather danger, so an upward trend means conditions conducive to fire have become more common/intense in the selected period.")
      }
    )
  })
  
  # Table of filtered data
  output$tbl <- renderDT({
    m <- input$metric
    out <- rng()[, c("year", m)]
    colnames(out) <- c("Year", toupper(m))
    datatable(out, rownames = FALSE, options = list(pageLength = 8))
  })
}

shinyApp(ui, server)
