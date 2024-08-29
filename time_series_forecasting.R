temp <- tempfile(fileext = ".csv")
download.file("https://drive.google.com/uc?export=download&id=1t8tVsqaAFiiPbHKVO5fYYXidYyDE63bI",temp)
data = read.csv(temp)

## order creation analysis 

creation = ts(data$created,frequency = 1)
creation
plot(creation)
library(tidyverse)
ggplot(data, aes(x= dat, y= created )) + geom_point()+geom_smooth()


ggplot(data,aes(x = dat,y = processed)) + geom_point()

ggplot(data,aes(x = dat,y = delivered)) + geom_point()+geom_line()

ggplot(data,aes(x = dat,y =returned)) + geom_point()
decompose(data)


ggplot(mtcars,aes(x = mpg,y = am))+geom_line()


data = data %>% 
  mutate(date = as.Date(dat)) %>% 
  select(-dat)
### xgboost performing
data = data %>%  relocate(date,.before = created)

library(caret)
library(xgboost)  # Make sure the xgboost package is installed

# Assuming "created" is the dependent variable and "dat" contains independent variables
train_data <- data[1:1500, ]
test_data <- data[1501:1541, ]

# Specify the formula for the model
formula <- created ~ .  # Assuming you want to use all columns in 'dat' as predictors

# Train the XGBoost model using caret
model <- train(formula, data = train_data, method = "xgbTree")

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

plot(test_data$date,predictions,type ="l")
lines(test_data$date,test_data$created,col = "red")
# Print or further analyze the predictions
print(predictions)









