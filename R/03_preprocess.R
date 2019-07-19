
# source("R//01_load_pkgs.R")
# 
# paste0(
#   "R/", 
#   list.files("R/", "2_")) %>% 
#   walk(source)

# Funtion for preprocessing data ------------------------------------------

dd_preprocess <- function (
  
  dt = data,           # Complete-case data with binary (0, 1) response y
                       # and binary main effects (0, 1) 
  exclude_n = 5,       # Exclude if < expected count
  log_odd_limit = 0.3, # Absolute log odds ratio limit
  test_ctrls = FALSE)  # Test log odds ratio crterion in controls or total data
  
{
  
  # All 2-way interactions
  x = model.matrix(~ .^2-1, data = dt %>% select(-y)) # -1 to exclude intercept
  x <- as.matrix(x)

  # Rename interactions
  colnames(x) <- colnames(x) %>%
    str_replace(":", "_x_")
  
  dt <- 
    cbind(dt %>% select(y), x) %>% 
    as.tibble(rownames = NULL)
  
  # Exlcude predictors with low expected counts
  
  exp_count <- 
    matrix(
      NA, 
      ncol = 6, 
      nrow = ncol(dt) - 1
    )
  
  for(i in 1:(ncol(dt) - 1)) {
    
    # Cross tab predictor * binary y
    temp_tab <- xtabs( 
      as.formula(
        paste(
          " ~ y +", 
          colnames(dt[, -1])[i]
        )
      ), 
      data = dt)
    
    ifelse(
      ncol(temp_tab) == 2, 
      exp_count[i, ] <-     # Expected counts
        (
          outer(
            rowSums(temp_tab), 
            colSums(temp_tab), 
            "*"
          )
          /
            margin.table(temp_tab)
        )[, 2], 
      exp_count[i, ] <- c(0, 0) # Mark with 0s if 0s in cross xtabs
    )
    
    # Add actual counts
    ifelse(
      ncol(temp_tab) == 2, 
      exp_count[i, 3:4] <- 
        temp_tab[, 2], 
      exp_count[i, 3:4] <- c(0, 0)
    )
    
  }

  # Add prevalence
  exp_count[, 5] <- 
    exp_count[, 3] / nrow(dt[dt$y == 0, ]) * 100
  exp_count[, 6] <- 
    exp_count[, 4] / nrow(dt[dt$y == 1, ]) * 100
    
  # Make dataframe showing expected counts
  
  colnames(exp_count) <- 
    c("expected_count_y0", "expected_count_y1", 
      "count_y0", "count_y1", 
      "prev_y0", "prev_y1")
  
  exp_count <- 
    exp_count %>% 
    as.tibble() %>% 
    mutate(
      predictor = colnames(dt[, -1]), 
      include = 
        expected_count_y0 >= exclude_n &
        expected_count_y1 >= exclude_n
    ) %>% 
    select(
      predictor, 
      count_y0, count_y1, prev_y0, prev_y1, 
      everything()
    )
  
  # Predictors with expected count meeting criterion
  
  dt <- 
    dt %>% 
    select(
      everything(),    # needed for "-contains" to work
      -contains("_x_"), 
      one_of(
        exp_count[exp_count$include == TRUE, ]$predictor %>% 
          as.character()
      )
    )
  
  # Exclude interactions of variables with high log(OR): 
  
  excl_minimum_n <- 
    exp_count %>%
    filter(str_detect(predictor, "_x_")) %>% # Restrict to interactions;
    separate(
      predictor,
      sep = "_x_",
      into = c("Var_1", "Var_2"),
      remove = FALSE
    ) %>%
    mutate(possible_interaction_1 = paste(Var_1, "_x_", Var_2, sep = "")) %>%
    mutate(possible_interaction_2 = paste(Var_2, "_x_", Var_1, sep = "")) %>% 
    filter(include == FALSE) %>%
    select(-include)
  
  cor_table <- # Pearson correlation
    dt %>%
    select(
      everything(),    # needed for "-contains" to work
      -contains("_x_"), 
      - y
    ) %>% 
    cor %>%
    round(2) %>%
    melt() %>% 
    mutate(possible_interaction = paste(Var1, "_x_", Var2, sep = "")) %>%
    mutate(excluded_interaction = ifelse(
      possible_interaction %in% excl_minimum_n$possible_interaction_1 |
        possible_interaction %in% excl_minimum_n$possible_interaction_2,
      1, 0
    )) %>%
    filter(excluded_interaction == 0) %>% 
    select(-excluded_interaction)
  
  for (i in 1:(nrow(cor_table))) { # Add log(OR) to correlation table
    
    temp_2x2 <- 
      xtabs(
        as.formula(
          paste(
            " ~ ", 
            cor_table[i, 1],  # One variable
            " + ", 
            cor_table[i, 2]   # Another variable
          )
        ), 
        dt %>% 
          filter(
            if(test_ctrls == TRUE) y==0 else y %in% c(0,1)
          )
      )
    
    cor_table[i, "odds_ratio"] <- 
      ifelse(
        temp_2x2 %>% ncol() == 2, 
        (temp_2x2[2, 2] * temp_2x2[1, 1]) / 
          (temp_2x2[2, 1] * temp_2x2[1, 2]), 
        NA
      )
    
    cor_table[i, "log_odds_ratio"] <- cor_table[i, "odds_ratio"] %>% 
      log() %>% 
      round(2)
    
  }
  
  cor_table <- 
    cor_table %>% 
    as.tibble() %>% 
    mutate(
      include = log_odds_ratio < abs(log_odd_limit)
    )
  
  # Predictors meeting log odds ratio criterion
  
  dt <- 
    suppressWarnings(
      dt %>% 
        select(
          -one_of(
            cor_table %>% 
              filter(include == FALSE) %>% 
              select(possible_interaction) %>% 
              pull()
          )
        ) 
    ) 
  
  # Return objects
  
  res <- list()
  
  res$data <- dt %>% as.tibble()
  res$exp_count <- exp_count %>% as.tibble()
  res$cor_table <- cor_table %>% as.tibble()
  
  return(res)
}

# Test --------------------------------------------------------------------

# data <- dd_sim(seed = 1)
# data
# proc_data <- dd_preprocess()
# proc_data
