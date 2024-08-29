## dataset 
library(mlbench)
data("PimaIndiansDiabetes")
library(tidyverse)
library(AER)
install.packages("modelsummary")
library(modelsummary)
PimaIndiansDiabetes %>% 
  head() 

PimaIndiansDiabetes %>% 
  select(diabetes) %>% 
  group_by(diabetes) %>% 
  count()
  



### two stage model



## manual way 

stage_1 = lm(glucose ~ insulin, data = PimaIndiansDiabetes)
stage_1
fit = fitted(stage_1)
stage_2 = lm(pressure ~ fit ,data = PimaIndiansDiabetes)
summary(stage_2)
broom::glance(stage_2)
## using faster way using package 

iv_model = ivreg(pressure ~ glucose | insulin, data = PimaIndiansDiabetes)
iv_model

### 
library(broom)
augment_columns(stage_1,PimaIndiansDiabetes)

## comparing model 
modelsummary(list(stage_1,stage_2,iv_model))
