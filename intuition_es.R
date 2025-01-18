if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, tidylog, here, pins, janitor, shiny)

ui <- fluidPage(
  titlePanel("Comparing Two Distributions"),
  sidebarLayout(
    sidebarPanel(
      h3("Distribution 1"),
      sliderInput("sample_size1", "Sample Size:", 
                  min = 10, max = 1000, value = 100, step = 10),
      sliderInput("mean1", "Mean:", 
                  min = 0, max = 5, value = 1, step = 0.1),
      sliderInput("std_dev1", "Standard Deviation:", 
                  min = 0.5, max = 5, value = 1, step = 0.1),
      
      h3("Distribution 2"),
      sliderInput("sample_size2", "Sample Size:", 
                  min = 10, max = 1000, value = 100, step = 10),
      sliderInput("mean2", "Mean:", 
                  min = 0, max = 5, value = 2, step = 0.1),
      sliderInput("std_dev2", "Standard Deviation:", 
                  min = 0.5, max = 5, value = 1, step = 0.1)
    ),
    mainPanel(
      plotOutput("violinPlot"),
      plotOutput("boxPlot"),
      h3("P-Value from t-test"),
      textOutput("pValue"),
      h3("Effect Size (Cohen's d)"),
      textOutput("effectSize")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expressions to generate data for both distributions
  data1 <- reactive({
    rnorm(input$sample_size1, mean = input$mean1, sd = input$std_dev1)
  })
  
  data2 <- reactive({
    rnorm(input$sample_size2, mean = input$mean2, sd = input$std_dev2)
  })
  
  # Combine data for plotting
  combinedData <- reactive({
    data.frame(
      value = c(data1(), data2()),
      group = rep(c("Distribution 1", "Distribution 2"), 
                  c(input$sample_size1, input$sample_size2))
    )
  })
  
  # Render violin plot
  output$violinPlot <- renderPlot({
    ggplot(combinedData(), aes(x = group, y = value, fill = group)) +
      geom_violin(alpha = 0.7) +
      labs(title = "Violin Plot of Two Distributions", x = NULL, y = "Value") +
      theme_minimal() +
      scale_fill_manual(values = c("skyblue", "orange"))
  })
  
  # Render box plot
  output$boxPlot <- renderPlot({
    ggplot(combinedData(), aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "Box Plot of Two Distributions", x = NULL, y = "Value") +
      theme_minimal() +
      scale_fill_manual(values = c("skyblue", "orange"))
  })
  
  # Calculate and display p-value from a t-test
  output$pValue <- renderText({
    t_test <- t.test(data1(), data2())
    paste("P-Value:", signif(t_test$p.value, digits = 4))
  })
  
  # Calculate and display Cohen's d
  output$effectSize <- renderText({
    x <- data1()
    y <- data2()
    
    n1 <- length(x)
    n2 <- length(y)
    
    # Pooled standard deviation
    pooled_sd <- sqrt(((n1 - 1)*sd(x)^2 + (n2 - 1)*sd(y)^2) / (n1 + n2 - 2))
    
    d <- (mean(y) - mean(x)) / pooled_sd
    
    paste("Effect Size (Cohen's d):", round(d, 3))
  })
}

shinyApp(ui = ui, server = server)
