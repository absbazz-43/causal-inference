# Load required libraries
library(shiny)
library(dplyr)
library(xgboost)
library(caret)
library(ggplot2)
library(readxl)

# Define UI
ui <- fluidPage(
  titlePanel("XGBoost Forecasting Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file",
                accept = c(".xlsx")
      ),
      actionButton("goButton", "Run Forecasting")
    ),
    mainPanel(
      plotOutput("forecastPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  observeEvent(input$goButton, {
    df_xautry <- data()
    df_xautry$date <- as.Date(df_xautry$date)
    
    train <- df_xautry[df_xautry$date < "2021-01-01",]
    test <- df_xautry[-(1:nrow(train)),]
    
    train_data <- train %>% select(xe, xau_usd_ounce)
    test_data <- test %>% select(xe, xau_usd_ounce) 
    
    targets <- train$xau_try_gram
    
    xgb_trcontrol <- trainControl(
      method = "cv", 
      number = 10,
      allowParallel = TRUE, 
      verboseIter = FALSE, 
      returnData = FALSE
    )
    
    xgb_grid <- base::expand.grid(
      list(
        nrounds = seq(100,200),
        max_depth = c(6,15,20), 
        colsample_bytree = 1, 
        eta = 0.5,
        gamma = 0,
        min_child_weight = 1,  
        subsample = 1)
    )
    
    model_xgb <- caret::train(
      train_data,targets,
      trControl = xgb_trcontrol,
      tuneGrid = xgb_grid,
      method = "xgbTree",
      nthread = 10
    )
    
    fitted_train <- model_xgb %>%
      stats::predict(train_data) %>%
      stats::ts(start = c(2013,1),frequency = 12)
    
    fitted_test <- model_xgb %>% 
      predict(test_data) %>% 
      ts(start = c(2021,1),frequency = 12)
    
    mm <- data.frame(date = c(train$date, test$date),
                     actual = c(train$xau_try_gram, test$xau_try_gram),
                     response = c(fitted_train, fitted_test))
    
    output$forecastPlot <- renderPlot({
      ggplot(mm) +
        geom_line(aes(x = date, y = actual, color = "Actual"), size = 1) +
        geom_line(aes(x = date, y = round(response), color = "Forecasting"), size = 1) +
        geom_vline(xintercept = min(test$date), linetype = "dashed", color = "yellow", size = 1) +
        labs(color = "Legend Title") +
        scale_color_manual(values = c("Actual" = "green", "Forecasting" = "red"))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
