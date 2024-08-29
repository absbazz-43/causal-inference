
# Load required libraries
library(shiny)
library(xgboost)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("XGBoost Forecasting Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file", accept = c(".csv")),
      selectInput("ycol", "Select Y Variable", "")
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    updateSelectInput(session, "ycol", choices = names(data()), selected = names(data())[1])
  })
  
  output$plot1 <- renderPlot({
    n <- nrow(data())
    
    synthetic_data <- data.frame(
      date = as.Date(data()$dat),
      value = data()[, input$ycol]
    )
    
    train_size <- floor(0.95 * n)
    train_data <- synthetic_data[1:train_size, ]
    test_data <- synthetic_data[(train_size + 1):n, ]
    
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data[-1]), label = train_data$value)
    test_matrix <- xgb.DMatrix(data = as.matrix(test_data[-1]))
    
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "mae",
      max_depth = 100,
      eta = 0.01,
      nrounds = 1000
    )
    
    xgb_model <- xgb.train(
      params = params,
      data = train_matrix,
      nrounds = 1000
    )
    
    predictions <- predict(xgb_model, newdata = test_matrix)
    
    plot_data <- data.frame(date = test_data$date, actual = test_data$value, predicted = round(predictions))
    
    ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = actual, color = "Actual"), size = 1) +
      geom_line(aes(y = predicted, color = "Predicted"), size = 1) +
      labs(color = "Legend Title") +
      scale_color_manual(values = c("Actual" = "green", "Predicted" = "red"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
