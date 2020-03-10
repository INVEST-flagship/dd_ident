#######################################
# NB Computationally extensive
#######################################

source("R//01_load_pkgs.R")
paste0(
  "R/",
  list.files("R/", "^0[2-7]_|^1[01]_")) %>%
  walk(source)


# Defined intercept
def_intercept <- 
  read_rds("tables/intercept_3int_20200221164354.rds") %>% 
  select(
    contains("prev"), 
    contains("OR"), 
    correlation, 
    scenario, 
    intercept) %>% 
  slice(1, 4, 8) # Scope

# Arguments for simulation ------------------------------------------------

# Number of simulations; can be multiplied with n_files, see 'Return results'.
# If a large number of simulation studies are conducted, it is recommended to 
# use n_sim < 100. The total number of simulations can then be increased by
# looping the simulations over more files (see below at 'Return results') 
n_sim <- 100

arg_int <- 
  def_intercept %>% 
  right_join(
    expand.grid(
      scenario = def_intercept %>% select(scenario) %>% pull(), 
      
      # Arguments for preprocessing
      exclude_n = 5, 
      log_odd_limit = 0.3, #c(0.1, 0.5), # seq(0.1, 0.7, 0.2)
      
      n_sim = 1:n_sim     # number of simulations
    ), 
    by = "scenario"
  )

# Arguments for preprocessing as list to work in future_pmap
arg_int_list <- 
  list(
    prev2 = as.list(arg_int$prev2), 
    OR_int = as.list(arg_int$OR_int), 
    intercept = as.list(arg_int$intercept), 
    correlation = as.list(arg_int$correlation), 
    
    exclude_n = as.list(arg_int$exclude_n), 
    log_odd_limit = as.list(arg_int$log_odd_limit)
  )

# Helper function calling both dd_sim and dd_preprocess -------------------

dd_sim_preprocess_3 <- function(
  
  # Arguments for dd_sim_3
  n = 4500,                # Number of subjects
  p_group = 3,             # Number of groups of variables
  prev1 = 0.15, 
  prev2 = 0.15,
  prev3 = 0.15,            
  correlation = 0.3,       # Correlation in the groups of variables
  OR_int = 3.0,            # OR of active interactions
  intercept = -0.95,       # Intercept of model
  increase_pred = 0,       # Add noise variables in each group of variables
  seed = NULL, 
  
  # Arguments for dd_preprocess
  exclude_n = 5,      
  log_odd_limit = 0.3
  
) {
  
  dd_preprocess(
    dd_sim_3(
      n = n, 
      p_group = p_group,
      prev1 = prev1, 
      prev2 = prev2,
      prev3 = prev3,
      correlation = correlation,
      # OR_main = OR_main,
      OR_int = OR_int,
      intercept = intercept,
      increase_pred = increase_pred,
      seed = NULL
    ), 
    exclude_n = exclude_n,      
    log_odd_limit = log_odd_limit
  )$data
  
}

# Test
# dd_sim_preprocess_3()
# Add groups of variables and noise variables:
# dd_sim_preprocess_3(p_group = 4, increase_pred = 1)


# Multiprocess ------------------------------------------------------------

plan(
  multiprocess(
    workers = 3 # Choose manually number of cores
    # availableCores()-1 # Use all available cores except one
  )
)

# Check computational time
tic()

# Loop simulations over multiple files and write files to save central memory
n_files <- 1 # n_files * n_sim = number of simulations

for(i in 1:n_files) {
  
  res_3act_int <- 
    arg_int %>% 
    mutate(
      data = future_pmap(arg_int_list, dd_sim_preprocess_3), 
      res_elast = future_map(data, dd_select_3_act_int), 
      data = NULL
    )
  
  
  write_rds(
    res_3act_int,
    paste0(
      "tables//res_3act_sim_", i , "_",
      dd_timestamp(),
      ".rds")
  )
  
}

toc()

# Combine files
# write_rds(
#   paste0("tables//", list.files("tables", "res_3act_sim_[0-9]")) %>%
#     map_df(read_rds),
#   paste0(
#     "tables//res_3act_sim_combine_",
#     n_sim*n_files,
#     "_",
#     dd_timestamp(),
#     ".rds")
# )

