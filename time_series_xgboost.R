df_xautry = xautry_reg
df_xautry$date <- as.Date(df_xautry$date)
#Splitting train and test data set
train <- df_xautry[df_xautry$date < "2021-01-01",]
test <- df_xautry[-(1:nrow(train)),]


#Transform train and test data to DMatrix form
library(dplyr)
library(xgboost)

train_data  <- train %>% 
  select(xe, xau_usd_ounce)


test_data <- test %>% 
  select(xe, xau_usd_ounce) 

targets <- train$xau_try_gram

targets
head(df_xautry)


#Cross-validation
library(caret)

xgb_trcontrol <- trainControl(
  method = "cv", 
  number = 10,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)



#Building parameters set
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
fitted_train


fitted_test = model_xgb %>% 
  predict(test_data) %>% 
  ts(start = c(2021,1),frequency = 12)

mm = data.frame(date = c(train$date,test$date),actual = c(train$xau_try_gram,test$xau_try_gram), response = c(fitted_train,fitted_test) )

ggplot(mm) +
  geom_line(aes(x = date, y = actual, color = "Actual"), size = 1) +
  geom_line(aes(x = date, y = round(response), color = "Forecasting"), size = 1) +
  geom_vline(xintercept = min(test$date), linetype = "dashed", color = "yellow", size = 1) +
  labs(color = "Legend Title") +  # Add a legend title
  scale_color_manual(values = c("Actual" = "green", "Forecasting" = "red"))  # Set custom colors for the lines

