library(shiny)
library(ggplot2)
library(bslib)
library(DT)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cosmo"),
  card(
    card_header("Fire Weather Explorer"),
    card_body(
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("Controls"),
          selectInput("metric","Metric", choices = c("FWI"="fwi","MSR"="msr")),
          sliderInput("yr","Year range", min = 1979, max = 2020, value = c(1990,2020)),
          helpText("FWI/MSR are indices of fire weather danger.")
        ),
        card(
          card_header(textOutput("title")),
          plotOutput("ts_plot", height = 300),
          hr(), DTOutput("tbl")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load dataset from the package namespace (works during docs build too)
  dat <- get("se_aus_fwi", envir = asNamespace("aussiefirewx"))
  rng <- reactive(subset(dat, year >= input$yr[1] & year <= input$yr[2]))
  
  output$title <- renderText(sprintf("Trend: %s (%d–%d)",
                                     toupper(input$metric), input$yr[1], input$yr[2]))
  
  output$ts_plot <- renderPlot({
    m <- input$metric
    ggplot(rng(), aes(year, .data[[m]])) +
      geom_line(linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = .7) +
      labs(x = "Year", y = toupper(m))
  })
  
  output$tbl <- renderDT(DT::datatable(rng(), rownames = FALSE, options = list(pageLength = 8)))
}

shinyApp(ui, server)

