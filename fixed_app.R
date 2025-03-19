# Improved Version of 'Hello Shiny' with bslib and Card System + value boxes

library(shiny)
library(ggplot2)
library(bslib)
library(shinythemes)


# Define UI ---------------------------------------------------------------

ui <- 
  page_fluid(
    theme = bs_theme(bootswatch = "journal"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 25),
        selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
        selectInput("theme", "Choose a theme:", choices = c("Classic", "Minimal", "Dark"))
      ),
      layout_columns(
        col_widths = c(4,4,4),
        value_box("Mean Waiting Time", textOutput("mean_waiting")),
        value_box("Median Waiting Time", textOutput("median_waiting")),
        value_box("Total Eruptions", textOutput("eruption_count"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Histogram"),
          full_screen = T,
          card_body(plotOutput("distPlot"))
        ),
        card(
          card_header("Density Plot"),
          full_screen = T,
          card_body(plotOutput("densityPlot"))
        ),
        card(
          card_header("Box Plot"),
          full_screen = T,
          card_body(plotOutput("boxPlot"))
        ),
        card(
          card_header("Scatter Plot"),
          full_screen = T,
          card_body(plotOutput("scatterPlot"))
        )
      )
    )
  )


# Define server logic -----------------------------------------------------
server <- function(input, output) {
  theme_choice <- reactive({
    switch(input$theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  output$mean_waiting <- renderText({
    paste(round(mean(faithful$waiting), 2), "mins") # bug corrected: case of field name in dataset
  })
  
  output$median_waiting <- renderText({
    paste(round(median(faithful$waiting), 2), "mins")
  })
  
  output$eruption_count <- renderText({
    # paste(nrow(faithful)) # bug corrected: case of dataset name
    tryCatch({ paste(nrow(Faithful)) }, 
             error = function(e) { 
               showNotification("Error in Total Eruptions: no data available", type = "error") ""
               "N/A"
                })
  })
  
  
  output$distPlot <- renderPlot({
    # browser()
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1) # bugs corrected: input$bins should be added to a numeric value; starting and end values of the sequence should refer to the data (x)
    
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(breaks = bins, fill = input$color, color = "white") +
      labs(x = "Waiting time to next eruption (in mins)", y = "Frequency") +
      theme_choice()
  })
  
  output$densityPlot <- renderPlot({
    x <- faithful$waiting # bug corrected: case of field name in dataset
    
    ggplot(data.frame(x), aes(x)) +
      geom_density(fill = input$color, alpha = 0.5) +
      labs(title = "Density Plot of Waiting Times", X = "Waiting Time (mins)", Y = "Density") +
      theme_choice()
  })
  
  output$boxPlot <- renderPlot({
    x <- faithful$waiting
    
    ggplot(data.frame(x), aes(y = x)) +
      geom_boxplot(fill = input$color, color = "black") +
      labs(title = "Boxplot of Waiting Times", y = "Waiting Time (mins)") +
      theme_choice()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
      geom_point(color = input$color) +
      labs(title = "Scatter Plot of Eruptions vs Waiting Time", x = "Eruption Duration (mins)", y = "Waiting Time (mins)") +
      theme_choice() # bug corrected: calling of theme function missing parentheses
  })
}


# Run the application -----------------------------------------------------
shinyApp(ui, server, options = list(display.mode = "showcase"))