# source("R//01_load_pkgs.R")
# source("R//10_simdata_3int.R")

# Function for data including *three* active interactions
dd_select_3_act_int <- 
  function(
    dt = data, 
    sign_level = 0.05,
    a_pen = 0.75, 
    min_rule = FALSE, 
    loss = "auc") {
    
    out <- dd_select(dt = dt, 
                     a_pen = a_pen, 
                     min_rule = min_rule,
                     loss = loss)
    
    tab <- 
      out$bonf %>% 
      mutate(
        type = case_when(
          str_detect(Predictor, "_x_") ~ "int", 
          TRUE ~ "main"),
        active = case_when(
          # type == "main" & str_detect(Predictor, "1") ~ 1, 
          str_detect(Predictor, "A1_x_B1") ~ 1, 
          str_detect(Predictor, "A2_x_C2") ~ 1, 
          str_detect(Predictor, "B3_x_C3") ~ 1, 
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
        n
      )
    
    return(tab)
    
  }

# Test
# dd_select_3_act_int(dd_preprocess(dd_sim_3())$data)
