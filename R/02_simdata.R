
# Simulate data -----------------------------------------------------------

# source("R//01_load_pkgs.R")

dd_sim <- function (
  n = 4500,                # Number of subjects
  p_group = 3,             # Number of groups of variables
  prev1 = 0.20, 
  prev2 = 0.15,
  prev3 = .05,             # Prevalence of first three variables in each group 
  # of variables
  correlation = 0.3,       # Correlation in the groups of variables
  OR_main = 1.3,           # OR of main effects
  OR_int = 3.0,            # OR of active interaction
  intercept = -0.88,       # Intercept of model
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
  
  # Main effects set to OR_main
  main <- x %>% 
    select(contains("1")) %>% 
    colnames() 
  
  x <- as.matrix(x)
  
  b_list <- list()
  
  for(i in main) {
    b <- log(OR_main)*x[,  i]
    b_list[[i]] <- b
  }
  
  b <- Reduce("+", b_list)
  
  # Add intercept and interaction effect
  b <- intercept + b + log(OR_int)*x[,  "A2"]*x[,  "B2"]
  
  # Inverse logit function
  prob = 1/(1 + exp(-b)) 
  
  y = rbinom(n, 1, prob)
  
  # Return data
  
  data = cbind(y, x) %>% as_tibble()
  
  return(data)
  
}
