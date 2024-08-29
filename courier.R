# Load required libraries
library(xgboost)
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
temp <- tempfile(fileext = ".csv")
download.file("https://drive.google.com/uc?export=download&id=1t8tVsqaAFiiPbHKVO5fYYXidYyDE63bI",temp)
data = read.csv(temp)

# Generate synthetic time series data
n <-length(data$created)# Number of data points
date_sequence <- seq(as.Date("2022-01-01"), by = "days", length.out = n)
synthetic_data <- data.frame(
  date = date_sequence,
  value = data$created
)

# Split the data into training and test sets
train_size <- floor(0.8 * n)
train_data <- synthetic_data[1:train_size, ]
test_data <- synthetic_data[(train_size + 1):n, ]

# Prepare the data for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[-1]), label = train_data$value)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[-1]))

# Set parameters for XGBoost
params <- list(
  objective = "reg:squarederror",
  eval_metric = "mae",
  max_depth = 100,
  eta = 0.01,
  nrounds = 1000
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 1000
)
# Make predictions on the test set
predictions <- predict(xgb_model, newdata = test_matrix)

# Plot the actual vs. predicted values
plot_data <- data.frame(date = test_data$date, actual = test_data$value, predicted = round(predictions))
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 1) +
  labs(color = "Legend Title") +
  scale_color_manual(values = c("Actual" = "green", "Predicted" = "red"))



