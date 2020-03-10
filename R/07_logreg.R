
# source("R//01_load_pkgs.R")
# paste0(
#   "R/", 
#   list.files("R/", "^0[2-6]_")) %>% 
#   walk(source)


# Function for marginal screening of interactions and main effects --------

# Helper functions

logregr_glm <- 
  function(formula, data = data) {
    expr <- rlang::expr(glm(!!formula, 
                            family = "binomial",
                            data = data))
    rlang::eval_tidy(expr)
  }

p_dd_main <- # P-value form models including only main effects (row 2)
  function(model) {
    summary(model)$coefficients[2,4]
  }

p_dd_int <- # P-value form models including an interaction effects (row 4)
  function(model) {
    summary(model)$coefficients[4,4]
  }

dd_lr <- 
  function(dt = data) {
    
    dt <- dt %>% 
      mutate(y = factor(y))
    
    form_list <- 
      
      bind_rows(
        
        # Formulas for main effects
        tibble(
          V1 = NA,
          V2 = NA,
          V3 = dt %>%
            select(-contains("_x_"), -y) %>%
            colnames(),
          formula =
            paste(
              "y ~", V3
            )
        ), 
        
        # Formulas for interaction models
        dt %>%
          select(contains("_x_")) %>%
          colnames() %>%
          str_split("_x_", simplify = T) %>%
          as_tibble() %>%
          mutate(V3 = paste0(V1, "_x_", V2),
                 formula =
                   paste(
                     "y ~", V1, "+", V2, "+", V3
                   )
          )
        
      ) 
    
    form_list <- 
      form_list %>%
      mutate(fit = map(formula, logregr_glm, data = dt))
    
    return(form_list)
    
  }

# Test
# dd_lr(
#   dt = dd_preprocess(
#     dd_sim(seed = 100),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data
# )

# Extract true and false positives in simulation data ---------------------

# Simulation data with active interaction

dd_lr_act_int <- function(dt = data) {
  
  tab <- dd_lr(dt = dt) 
  
  tab_main <- # Main effects
    tab %>% 
    filter(!str_detect(V3, "_x_")) %>% 
    transmute(
      V3, 
      p_V3 = map_dbl(fit, p_dd_main),
      sign_V3 = ifelse(p_V3 < 0.05/nrow(tab), 1, 0), 
      active_main = ifelse(str_detect(V3, "1"), 1, 0)
    ) 
  
  tab_main <- 
    tab_main %>% 
    count(sign_V3, active_main) %>% 
    transmute(cell = 
                case_when(
                  sign_V3 == 1 & active_main == 1 ~ "TP_main", 
                  sign_V3 == 0 & active_main == 1 ~ "FN_main", 
                  sign_V3 == 1 & active_main == 0 ~ "FP_main", 
                  sign_V3 == 0 & active_main == 0 ~ "TN_main"
                ),
              n, 
              prob = 
                case_when(
                  cell == "TP_main" | cell == "FN_main" ~ 
                    n/nrow(tab_main %>% filter(active_main == 1)), 
                  cell == "FP_main" | cell == "TN_main" ~ 
                    n/nrow(tab_main %>% filter(active_main == 0))
                )
    )
  
  tab_int <- # Interactions
    tab %>% 
    filter(str_detect(V3, "_x_")) %>% 
    transmute(
      V3, 
      p_V3 = map_dbl(fit, p_dd_int),
      sign_V3 = ifelse(p_V3 < 0.05/nrow(tab), 1, 0), 
      active_int = ifelse(V3 == "A2_x_B2", 1, 0)
    )
  
  tab_int <-
    tab_int %>% 
    count(sign_V3, active_int) %>% 
    transmute(cell = 
                case_when(
                  sign_V3 == 1 & active_int == 1 ~ "TP_int", 
                  sign_V3 == 0 & active_int == 1 ~ "FN_int", 
                  sign_V3 == 1 & active_int == 0 ~ "FP_int", 
                  sign_V3 == 0 & active_int == 0 ~ "TN_int"
                ),
              n, 
              prob = 
                case_when(
                  cell == "TP_int" | cell == "FN_int" ~ 
                    n/nrow(tab_int %>% filter(active_int == 1)), 
                  cell == "FP_int" | cell == "TN_int" ~ 
                    n/nrow(tab_int %>% filter(active_int == 0))
                )
    )
  
  tab <- 
    bind_rows(tab_main, tab_int)
  
  return(tab)
}

# Test
# dd_lr_act_int(
#   dt = dd_preprocess(
#     dd_sim(seed = 100),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data)

# Same but without Bonferroni correction

dd_lr_act_int_nb <- function(dt = data) {
  
  tab <- dd_lr(dt = dt) 
  
  tab_main <- # Main effects
    tab %>% 
    filter(!str_detect(V3, "_x_")) %>% 
    transmute(
      V3, 
      p_V3 = map_dbl(fit, p_dd_main),
      sign_V3 = ifelse(p_V3 < 0.05, 1, 0), 
      active_main = ifelse(str_detect(V3, "1"), 1, 0)
    ) 
  
  tab_main <- 
    tab_main %>% 
    count(sign_V3, active_main) %>% 
    transmute(cell = 
                case_when(
                  sign_V3 == 1 & active_main == 1 ~ "TP_main", 
                  sign_V3 == 0 & active_main == 1 ~ "FN_main", 
                  sign_V3 == 1 & active_main == 0 ~ "FP_main", 
                  sign_V3 == 0 & active_main == 0 ~ "TN_main"
                ),
              n, 
              prob = 
                case_when(
                  cell == "TP_main" | cell == "FN_main" ~ 
                    n/nrow(tab_main %>% filter(active_main == 1)), 
                  cell == "FP_main" | cell == "TN_main" ~ 
                    n/nrow(tab_main %>% filter(active_main == 0))
                )
    )
  
  tab_int <- # Interactions
    tab %>% 
    filter(str_detect(V3, "_x_")) %>% 
    transmute(
      V3, 
      p_V3 = map_dbl(fit, p_dd_int),
      sign_V3 = ifelse(p_V3 < 0.05, 1, 0), 
      active_int = ifelse(V3 == "A2_x_B2", 1, 0)
    )
  
  tab_int <-
    tab_int %>% 
    count(sign_V3, active_int) %>% 
    transmute(cell = 
                case_when(
                  sign_V3 == 1 & active_int == 1 ~ "TP_int", 
                  sign_V3 == 0 & active_int == 1 ~ "FN_int", 
                  sign_V3 == 1 & active_int == 0 ~ "FP_int", 
                  sign_V3 == 0 & active_int == 0 ~ "TN_int"
                ),
              n, 
              prob = 
                case_when(
                  cell == "TP_int" | cell == "FN_int" ~ 
                    n/nrow(tab_int %>% filter(active_int == 1)), 
                  cell == "FP_int" | cell == "TN_int" ~ 
                    n/nrow(tab_int %>% filter(active_int == 0))
                )
    )
  
  tab <- 
    bind_rows(tab_main, tab_int)
  
  return(tab)
}

# Test
# dd_lr_act_int_nb(
#   dt = dd_preprocess(
#     dd_sim(seed = 100),
#     exclude_n = 5,
#     log_odd_limit = 0.3
#   )$data)

