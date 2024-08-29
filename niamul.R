library(tidyverse)
library(broom)
library(geepack)
library(xgboost)
library(caret)
library(randomForest)

gen_dat <- function(n = 1000){
  
  data <- tibble(
    id = 1:n,
    U = rbinom(n, 1, prob = 0.4),
    x = exp(0.25 - 0.2*U),
    p_hat = x / (1 + x),
    L1 = rbinom(n, 1, p_hat),
    x2 = exp(0.5 - 0.3*U),
    p_hat2 = x2 / (1 + x2),
    L2 = rbinom(n, 1, p_hat2),
    x3 = exp(0.4 - 0.3*L1 - 0.4*L2),
    p2_hat = x3/ (1 + x3),
    A = rbinom(n, 1, prob = p2_hat),
    y = rnorm(n, 0.2 - 0.12*A + 0.8*L1 + 0.6*L2 - 0.5*U, 0.2)) %>%
    select(id, U, L1, L2, A, y)
  
  
  
  ################# weight for treatment ###########################
  
  form_a_d <- formula(A ~ L1 + L2)
  
  mdl_a_d <- glm(form_a_d, data = data, family = "binomial")
  
  data <- augment(mdl_a_d, newdata = data, type.predict = "response") %>%
    mutate(
      wa = if_else(A == 1, 1/.fitted, 1/(1-.fitted))
    ) %>% select(-.fitted)
  
  
  # form_a_n <- formula(A ~ 1 )
  #
  # mdl_a_n <- glm(form_a_n, data = data, family = "binomial")
  #
  # data <- augment(mdl_a_n, newdata = data, type.predict = "response") %>%
  #   mutate(
  #     pa = if_else(A == 1, .fitted, 1- .fitted),
  #     sw_a = pa*wa
  #   ) %>% select(-c(.fitted, pa))
  #
  #
  
  model2 <- randomForest(A ~ L1 + L2, data = data, ntree = 500, importance = TRUE)
  
  predTrain <- predict(model2, data, type = "class")
  
  
  dat <- data %>% mutate(r = predTrain)
  
  dat_f <- dat %>%
    mutate(
      rn_w = if_else(A == 1, 1/r, 1/(1-r))
    )
  
  
  xgb.model <- train(
    A ~ L1 + L2, data = dat_f, method = "xgbTree",verbosity = 0,
    trControl = trainControl("cv", number = 50)
  )
  
  pred <- predict(xgb.model, dat_f)
  
  
  
  dat_final <- dat_f %>%
    mutate(
      bst_w = if_else(A == 1, 1/pred, 1/(1-pred))
    )
  
  return(dat_final)
}




##################### Ip_weighting ####################

Ip_w <- function(data){
  
  mod_gee <- geeglm(y ~ A ,
                    data = data,
                    id = id,
                    weights = wa,
                    corstr = "independence",
                    family = gaussian("identity")) %>% tidy()
  
  
  
  est <- mod_gee$estimate[2]
  std <- mod_gee$std.error[2]
  return(list(est, std))
  
}

############################ Random forest ################
rndm_f <- function(data){
  
  mod_gee_r <- geeglm(y ~ A ,
                      data = data,
                      id = id,
                      weights = rn_w,
                      corstr = "independence",
                      family = gaussian("identity")) %>% tidy()
  
  
  est <- mod_gee_r$estimate[2]
  std <- mod_gee_r$std.error[2]
  return(list(est, std))
  
  
}

################################ Xbboosting ################

xb <- function(data){
  
  mod_gee_b <- geeglm(y ~ A ,
                      data = data,
                      id = id,
                      weights = bst_w,
                      corstr = "independence",
                      family = gaussian("identity")) %>% tidy()
  
  
  est <- mod_gee_b$estimate[2]
  std <- mod_gee_b$std.error[2]
  return(list(est, std))
  
  
}





######### Simulated vale ####################

get_sim_se <- function(n, nboot = 1000, seed = 101) {
  
  set.seed(seed)
  
  real_value <- -0.12
  
  
  ip_mat_est <- matrix(NA, nrow = 1, ncol = nboot)
  
  ip_mat_sd <- matrix(NA, nrow = 1, ncol = nboot)
  
  rn_mat_est <- matrix(NA, nrow = 1, ncol = nboot)
  
  rn_mat_sd <- matrix(NA, nrow = 1, ncol = nboot)
  
  xb_mat_est <- matrix(NA, nrow = 1, ncol = nboot)
  
  xb_mat_sd <- matrix(NA, nrow = 1, ncol = nboot)
  
  
  for(i in 1:nboot) {
    
    
    
    message(paste0("Simulation run for ", i))
    
    dat_b <- gen_dat(n)
    
    
    
    ########### Ip-weighting #############
    
    estimt <- Ip_w(dat_b)  
    
    ip_mat_est[, i] <- unlist(estimt[[1]])
    
    ip_mat_sd[, i] <- unlist(estimt[[2]])
    
    
    ########## Random Forest ############
    
    est <- rndm_f(dat_b)
    
    rn_mat_est[, i] <- unlist(est[[1]])
    
    rn_mat_sd[, i] <- unlist(est[[2]])
    
    
    ######## Xb boosting ############
    
    est_b <- xb(dat_b)
    
    xb_mat_est[, i] <- unlist(est_b[[1]])
    
    xb_mat_sd[, i] <- unlist(est_b[[2]])
    
    
    
    
  }
  
  
  
  
  
  es_mn_ip <- round(apply(ip_mat_est,1, mean), 4)
  bias_1 <- round(es_mn_ip - real_value,4)
  r_bias1 <- round(abs(bias_1/ real_value), 4)*100
  es_sd_ip <- round(apply(ip_mat_est,1, sd), 4)
  es_se_ip <- round(apply(ip_mat_sd, 1, mean), 4)
  # output3 <- tibble(estimate = es_mn_ip, bias = bias_3, r_bias = r_bias3, std = es_sd_ip, se = es_se_ip)
  output1 <- tibble(bias = bias_1, std = es_sd_ip, se = es_se_ip, mse = round(bias^2 + std^2,4))
  
  
  es_mn_rn <- round(apply(rn_mat_est,1, mean), 4)
  bias_2 <- round(es_mn_rn - real_value, 4)
  r_bias2 <- round(abs(bias_2/ real_value), 4)*100
  es_sd_rn <- round(apply(rn_mat_est,1, sd), 4)
  es_se_rn <- round(apply(rn_mat_sd, 1, mean), 4)
  # output3 <- tibble(estimate = es_mn_ip, bias = bias_2, r_bias = r_bias3, std = es_sd_ip, se = es_se_ip)
  output2 <- tibble(bias = bias_2, std = es_sd_rn, se = es_se_rn, mse = round(bias^2 + std^2,4))
  
  
  
  es_mn_xb <- round(apply(xb_mat_est,1, mean), 4)
  bias_3 <- round(es_mn_xb - real_value, 4)
  r_bias3 <- round(abs(bias_3/ real_value), 4)*100
  es_sd_xb <- round(apply(xb_mat_est,1, sd), 4)
  es_se_xb <- round(apply(xb_mat_sd, 1, mean), 4)
  # output3 <- tibble(estimate = es_mn_ip, bias = bias_2, r_bias = r_bias3, std = es_sd_ip, se = es_se_ip)
  output3 <- tibble(bias = bias_3, std = es_sd_xb, se = es_se_xb, mse = round(bias^2 + std^2,4))
  
  
  
  
  
  # return(list(output1, output2, output3))
  return(purrr::list_cbind(
    list(output1, output2, output3)
  ))
}


# sink(file = "No_cen.txt")
#
# for(i in c(200, 500, 1000, 2000, 5000, 10000)){
#  
#   cat("sample size=",i)
#   print(get_sim_se(i, nboot = 500, seed = 1797))
#   cat("-------------","\n")
#  
# }
#    
# sink()


res <- purrr::map_dfr(.x = c(200, 500, 700),
                      .f = ~ get_sim_se(n = .x,
                                        nboot = 300,
                                        seed = 100))




res <- res %>%
  mutate(
    sample = rep(c(200, 500, 700), each = 1),
    par = rep(-0.12,3)
  ) %>%
  select(sample, par, everything())



kbl_output <- kableExtra::kbl(res, format = "latex")

cat(kbl_output, file = "random_forest_2L_new")