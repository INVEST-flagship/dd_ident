
# source("R//01_load_pkgs.R")
# 
# paste0(
#   "R/", 
#   list.files("R/", "^0[2-5]_")) %>% 
#   walk(source)

# Function for LASSO and post-selective inference -------------------------

dd_select <- function(
  dt = data,         # Preprocessed data 
  a_pen = 0.75,      # Alpha in elasticnet (elasticnet mixing parameter)
  sign_level = 0.05, # Significance level for confidence intervals
  min_rule = FALSE,  # If TRUE, minimum-rule to assess lambda. 
  # If FALSE (default), 1-SE-rule to assess lambda
  loss = "auc"       # Loss to use for cross-validation: "auc" or "deviance"
) {
  
  # Using preprocessed data
  proc_data <- dt 
  
  # Data to matrix
  x <- proc_data %>% 
    select(-y) %>% 
    as.matrix %>% 
    scale(T, F)
  
  y <- proc_data %>% 
    select(y) %>% 
    as.matrix()
  
  # Run LASSO
  
  gfit = glmnet(x = x, y = y, 
                alpha = a_pen,
                standardize=T, 
                family="binomial", 
                nlambda = 100
  )
  
  # Cross-validation to assess lambda
  cv.gfit = cv.glmnet(x = x, y = y, alpha = a_pen, 
                      standardize=T, family="binomial", 
                      nfolds = 10, 
                      type.measure = loss)
  
  lambda = ifelse(
    min_rule,
    cv.gfit$lambda.min, 
    cv.gfit$lambda.1se
  )
  
  # Extract betas at given lambda
  beta_hat = coef(gfit, s=lambda, exact=FALSE, 
                  x = x, y = y) 
  
  # Active predictors (beta not 0) at given lambda
  active = which(beta_hat != 0)
  
  # Coefficients of active predictors
  index = beta_hat[active]
  
  # Stop if no active predictors
  if(length(index) < 2) { # If < 2 -> Only intercept -> no active predictors
    
    # List to return
    out <- list()
    
    # Proportion cases
    out$prop_case <- length(y[y == 1]) / (length(y)) 
    
    # Number of main effects
    out$n_main <- 
      proc_data %>% 
      select(-y, -contains("_x_")) %>% 
      ncol()
    
    # Number of interactions
    out$n_int <- 
      proc_data %>% 
      select(contains("_x_")) %>% 
      ncol()
    
    # Empty table of Bonferroni corrected logistic regression
    out$bonf <- tibble(
      Predictor = as.character(NULL), 
      OR = as.numeric(NULL), 
      low_ci = as.numeric(NULL), 
      high_ci = as.numeric(NULL), 
      p = as.numeric(NULL)
    )
    
    return(out)
  }
  
  # Bonferroni corrected logistic regression
  boncorr_model = glm(y ~ ., data = as.data.frame(proc_data[, active]), 
                      family = "binomial") 
  
  boncorr_model_confint <- 
    suppressMessages(
      (boncorr_model %>% 
         confint(
           level = (
             1 - (sign_level / (ncol(proc_data) - 1))
           )
         )
      )
    ) 
  
  boncorr_table <- 
    tibble(
      Predictor = names(boncorr_model$coefficients[-1])
    ) %>% 
    mutate(
      OR = boncorr_model$coefficients[-1] %>% exp %>% round(2),
      low_ci = 
        boncorr_model_confint[-1, 1] %>% exp %>% round(2), 
      high_ci = 
        boncorr_model_confint[-1, 2] %>% exp %>% round(2), 
      p = 
        (boncorr_model %>% 
           summary %>% 
           coefficients)[-1,4] * (ncol(proc_data) - 1), 
      p = ifelse(p >= 1, 1, p) %>% round(3)
    )
  
  rownames(boncorr_table) <- NULL
  
  # Return objects
  
  out <- list()
  
  # Proportion cases
  out$prop_case <- length(y[y == 1]) / (length(y)) 
  
  # Number of main effects
  out$n_main <- 
    proc_data %>% 
    select(-y, -contains("_x_")) %>% 
    ncol()
  
  # Number of interaction effects
  out$n_int <- 
    proc_data %>% 
    select(contains("_x_")) %>% 
    ncol()
  
  # Output from selective inference using Bonferroni correction
  out$bonf <- boncorr_table
  
  return(out)
  
}

# Test alpha = 0.05
# set.seed(111)
# dd_select(
#   dd_preprocess(dd_sim(seed = 111, n = 4500))$data,
#   a_pen = 0.75,
#   sign_level = 0.05,
#   loss = "auc",
#   min_rule = F
#   )

# Test more exploratory alpha = 0.10
# set.seed(111)
# dd_select(
#   dd_preprocess(dd_sim(seed = 111, n = 4500))$data,
#   a_pen = 0.75,
#   sign_level = 0.1,
#   loss = "auc",
#   min_rule = F
# )


# Extract true and false positives in simulation data ---------------------

# Function for data including an active interaction
dd_select_act_int <- 
  function(
    dt = data, 
    a_pen = 0.75, 
    sign_level = 0.05,
    min_rule = FALSE, 
    loss = "auc") {
    
    out <- dd_select(dt = dt, 
                     a_pen = a_pen, 
                     sign_level = sign_level,
                     min_rule = min_rule,
                     loss = loss)
    
    tab <- 
      out$bonf %>% 
      mutate(
        type = case_when(
          str_detect(Predictor, "_x_") ~ "int", 
          TRUE ~ "main"),
        active = case_when(
          type == "main" & str_detect(Predictor, "1") ~ 1, 
          str_detect(Predictor, "A2_x_B2") ~ 1, 
          TRUE ~ 0
        ), 
        sign_p = if_else(p < sign_level, 1, 0)
      ) %>% 
      count(type, active, sign_p) %>% 
      transmute(
        cell = 
          case_when(
            type == "main" & active == 0 & sign_p == 0 ~ "TN_main", 
            type == "main" & active == 1 & sign_p == 0 ~ "FN_main", 
            type == "main" & active == 0 & sign_p == 1 ~ "FP_main", 
            type == "main" & active == 1 & sign_p == 1 ~ "TP_main", 
            type == "int" & active == 0 & sign_p == 0 ~ "TN_int", 
            type == "int" & active == 1 & sign_p == 0 ~ "FN_int", 
            type == "int" & active == 0 & sign_p == 1 ~ "FP_int", 
            type == "int" & active == 1 & sign_p == 1 ~ "TP_int"
          ), 
        n, 
        prob_among_selected = 
          case_when(
            cell == "TP_main" | cell == "FN_main" ~ 
              n/ (
                dt %>% select(contains("1")) %>% ncol()
              ), 
            cell == "FP_main" | cell == "TN_main" ~ 
              n/ (
                dt %>% select(-contains("1"), -contains("y")) %>% ncol()
              ),
            cell == "TP_int" | cell == "FN_int" ~  n/1, 
            cell == "FP_int" | cell == "TN_int" ~  n/(out$n_int-1)
          )
      )
    
    return(tab)
    
  }

# Test
# dd_select_act_int(
#   dt = dd_preprocess(
#     dd_sim(
#       n=4500,
#       correlation = 0.3,
#       OR_main = 1.3,
#       OR_int = 3,
#       intercept = -0.88,
#       increase_pred = 0),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data,
#   a_pen = 0.75, loss = "auc", min_rule = F
# )

# Test with specified seeds and alpha 5% and 10%
# set.seed(111)
# dd_select_act_int(
#   dt = dd_preprocess(
#     dd_sim(
#       n=4500,
#       seed = 111, 
#       correlation = 0.3,
#       OR_main = 1.3,
#       OR_int = 3,
#       intercept = -0.88,
#       increase_pred = 0),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data,
#   a_pen = 0.75, 
#   sign_level = 0.05, # alpha = 0.05
#   loss = "auc", 
#   min_rule = F
# )
# set.seed(111)
# dd_select_act_int(
#   dt = dd_preprocess(
#     dd_sim(
#       n=4500,
#       seed = 111, 
#       correlation = 0.3,
#       OR_main = 1.3,
#       OR_int = 3,
#       intercept = -0.88,
#       increase_pred = 0),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data,
#   a_pen = 0.75, 
#   sign_level = 0.1, # alpha = 0.10
#   loss = "auc", 
#   min_rule = F
# )
