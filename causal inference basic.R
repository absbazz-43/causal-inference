library(tidyverse)
library(magrittr)
# getting the data
data <- readr::read_csv(file = "https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv") %>% 
  mutate(
    education = case_when(
      education == 1  ~ "8th grade or less",
      education == 2 ~ "HS dropout",
      education == 3 ~ "HS",
      education == 4 ~ "College dropout",
      education == 5 ~ "College or more",
      T ~ "missing"
    )
  ) %>% 
  mutate(across(.cols = c(qsmk, sex, race, education, exercise, active), .fns = forcats::as_factor)) %>% 
  drop_na(qsmk, sex, race, education, exercise, active, wt82)
data

## save the data 

write.csv(data,file="nhfes.csv")

## the causal effect E(Y|A=1) - E(Y|A=0)

md = data %>% 
  group_by(qsmk) %>% 
  summarise("meanvalue"= mean(wt82_71)) 

md$meanvalue[2]-md$meanvalue[1]  
## fit a model 

mm=lm(wt82_71 ~ qsmk, data = data) %>% broom::tidy(.,conf.int = T) %>% 
  select (estimate, conf.low,conf.high)

mm
# IP weighting

# AS the treatment is dichotomous we fit a logistic regression at first 


treat_model = glm(qsmk ~ 1, family = binomial(link = "logit"), data = data)
treat_model_1 =  glm(data = data, formula = qsmk ~ sex + race + age + I(age*age) + education + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs + I(smokeyrs*smokeyrs) + exercise + active + wt71 + I(wt71*wt71), family = binomial("logit"))

data = data %>% 
  mutate(
    treat_model_r = predict(treat_model, type = "response"),
    treat_model_1_r = predict(treat_model_1, type = "response"),
    ipw = ifelse(qsmk==1, 1/treat_model_1_r, 1/(1- treat_model_1_r)),
    sbipw = ifelse(qsmk==1,treat_model_r/ treat_model_1_r,(1-treat_model_r)/(1-treat_model_1_r ))
  )


data %>% 
  group_by(qsmk) %>% 
  ggplot(aes(x = ipw, color = qsmk, fill = qsmk)) +
  geom_histogram(bins = 50) +
  theme_minimal() 



library(tidymodels)
set.seed(972188635)
boots <- bootstraps(data, times = 1e3, apparent = FALSE)
iptw_model_sw <- function(data) {
  glm(formula = wt82_71 ~ qsmk, family = gaussian(), weight = sbipw, data = data)
}
boot_models <- boots %>% 
  mutate(
    model = map(.x = splits, ~iptw_model_sw(data = .x)),
    coef_info = map(model, ~broom::tidy(.x)))
int_pctl(boot_models, coef_info) # M. Hernan: 3.4 (2.4-4.5)
