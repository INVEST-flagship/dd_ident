
# Simulate data with three active interactions ----------------------------

# source("R//01_load_pkgs.R")

# 3 active interactions ---------------------------------------------------

dd_sim_3 <- function (
  n = 4500,                # Number of subjects
  p_group = 3,             # Number of groups of variables
  # prev1 = 0.20,
  prev1 = 0.15,
  prev2 = 0.15,
  prev3 = 0.15,            # Prevalence of first three variables in each group 
  # of variables
  correlation = 0.3,       # Correlation in the groups of variables
  OR_int = 3.0,            # OR of active interactions
  intercept = -0.8,        # Intercept of model
  increase_pred = 0,       # Add noise variables in each group of variables
  seed = NULL)             # Specific seed for reprooducable examples
{
  # Simulate data
  set.seed(seed)
  
  if(p_group < 3) return("p_group must have value >= 3")
  
  # Correlated variables
  x_list <- list()
  
  for(i in 1:p_group) {
    
    x <- 
      simstudy::genCorGen(
        n, 
        nvars = 3 + increase_pred,
        params1 = c(prev1, prev2, prev3, 
                    runif(
                      increase_pred, 
                      min(c(prev1, prev2, prev3)), 
                      max(c(prev1, prev2, prev3)))
        ), 
        dist = "binary",
        rho = correlation,
        corstr = "cs",
        wide = TRUE,
        cnames = paste0(
          LETTERS[i], 1:(3 + increase_pred)
        )
      )
    
    x$id <- NULL
    
    x_list[[i]] <- x
    
  }
  
  x <- bind_cols(x_list)
  
  # Association with outcome
  
  x <- as.matrix(x)
  
  # Add intercept and interaction effect
  b <- 
    intercept + 
    # b +
    log(OR_int)*x[,  "A1"]*x[,  "B1"] +
    log(OR_int)*x[,  "A2"]*x[,  "C2"] +
    log(OR_int)*x[,  "B3"]*x[,  "C3"]

  # Inverse logit function
  prob = 1/(1 + exp(-b)) 
  
  y = rbinom(n, 1, prob)
  
  # Return data
  
  data = cbind(y, x) %>% as_tibble()
  
  return(data)
  
}
  
# Test
# dd_sim_3() %>% count(y)

