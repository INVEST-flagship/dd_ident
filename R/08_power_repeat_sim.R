
# source("R//01_load_pkgs.R")
# paste0(
#   "R/",
#   list.files("R/", "[2-7]_")) %>%
#   walk(source)

# Multiprocessing with furrr::future_map ----------------------------------

# Defined intercept
def_intercept <- 
  paste0("tables//", list.files("tables", "res_intercept")) %>% 
    map_df(read_rds) %>% 
  select(
    contains("prev"), 
    contains("OR"), 
    correlation, 
    scenario, 
    intercept)

# Scope
def_intercept <- 
  def_intercept %>%
  filter(
    correlation %in% c(0.1, 0.3, 0.5),
    prev2 >= .14 & prev2 <= .2,
    prev2 != .14,
    prev2 != .16,
    prev2 != .18,
    prev2 != .19,
    OR_main == 1.3,
    OR_int %in% c(3, 5)
    )
# def_intercept <- def_intercept[-c(5, 6), ]
def_intercept <- def_intercept[1, ]

stopifnot(def_intercept %>% distinct(scenario) %>% nrow() == nrow(def_intercept))

# Number of simulations; can be multiplied with n_files, see 'Return results'
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
  OR_main = as.list(arg_int$OR_main), 
  OR_int = as.list(arg_int$OR_int), 
  intercept = as.list(arg_int$intercept), 
  correlation = as.list(arg_int$correlation), 
  
  exclude_n = as.list(arg_int$exclude_n), 
  log_odd_limit = as.list(arg_int$log_odd_limit)
)


# Helper function calling both dd_sim and dd_preprocess
dd_sim_preprocess <- function(
  
  # Arguments for dd_sim
  n = 4500,
  prev1 = 0.20, 
  prev2 = 0.15,
  prev3 = .05,
  correlation = 0.3,
  OR_main = 1.3,
  OR_int = 3,
  intercept = -0.88,
  increase_pred = 0,
  seed = NULL, 
  
  # Arguments for dd_preprocess
  exclude_n = 5,      
  log_odd_limit = 0.3
  
) {
  
  dd_preprocess(
    dd_sim(
      n = n,
      prev1 = prev1, 
      prev2 = prev2,
      prev3 = prev3,
      correlation = correlation,
      OR_main = OR_main,
      OR_int = OR_int,
      intercept = -intercept,
      increase_pred = increase_pred,
      seed = NULL
    ), 
    exclude_n = exclude_n,      
    log_odd_limit = log_odd_limit
  )$data
  
}


# Return results ----------------------------------------------------------

# Save file for each 100 simulations. Mark file with timestamp

# Multiprocess
plan(multiprocess(workers = availableCores()-1))

# Check computational time
tic()

# Loop simulations over multiple files and write files to save central memory
n_files <- 100 # n_files * n_sim = number of simulations

for(i in 1:n_files) {
  
  res_act_int <- 
    arg_int %>% 
    mutate(
      data = future_pmap(arg_int_list, dd_sim_preprocess), 
      res_crude_nb = future_map(data, dd_lr_act_int_nb), 
      res_crude = future_map(data, dd_lr_act_int), 
      res_elast = future_map(data, dd_select_act_int), 
      data = NULL
    )
  
  
  write_rds(
    res_act_int,
    paste0(
      "tables//res_sim_", i , "_",
      dd_timestamp(),
      ".rds")
  )
  
}

toc()

# Combine files
write_rds(
  paste0("tables//", list.files("tables", "res_sim_[0-9]")) %>%
    map_df(read_rds),
  paste0(
    "tables//res_sim_combine_",
    n_sim*n_files, 
    "_",
    dd_timestamp(),
    ".rds")
)


# Non-active interaction --------------------------------------------------


# Defined intercept
def_intercept <- 
  paste0("tables//", list.files("tables", "res_intercept")) %>% 
  map_df(read_rds) %>% 
  select(
    contains("prev"), 
    contains("OR"), 
    correlation, 
    scenario, 
    intercept)

# Scope
def_intercept <- 
  def_intercept %>% 
  filter(
    prev2 >= .13 & prev2 <= .2,
    prev2 != .14,
    prev2 != .16,
    prev2 != .18,
    prev2 != .19,
    OR_main == 1.3,
    OR_int == 1
  )

def_intercept <- def_intercept[2, ]

stopifnot(def_intercept %>% distinct(scenario) %>% nrow() == nrow(def_intercept))

n_sim <- 100

arg_int <- 
  def_intercept %>% 
  right_join(
    expand.grid(
      scenario = def_intercept %>% select(scenario) %>% pull(), 
      
      # Arguments for preprocessing
      exclude_n = 5, 
      log_odd_limit = 0.3, # seq(0.1, 0.5, 0.1) 
      
      n_sim = 1:n_sim     # number of simulations
    ), 
    by = "scenario"
  )

# Arguments for preprocessing as list to work in future_pmap
arg_int_list <- 
  list(
    prev2 = as.list(arg_int$prev2), 
    OR_main = as.list(arg_int$OR_main), 
    OR_int = as.list(arg_int$OR_int), 
    intercept = as.list(arg_int$intercept), 
    correlation = as.list(arg_int$correlation), 
    
    exclude_n = as.list(arg_int$exclude_n), 
    log_odd_limit = as.list(arg_int$log_odd_limit)
  )


# Return results ----------------------------------------------------------

# Save file for each 100 simulations. Mark file with timestamp

# Multiprocess
plan(multiprocess(workers = availableCores()-1))

tic()

n_files <- 100

for(i in 1:n_files) {
  
  res_nonact_int <- 
    arg_int %>% 
    mutate(
      data = future_pmap(arg_int_list, dd_sim_preprocess), 
      res_crude_nb = future_map(data, dd_lr_nonact_int_nb), 
      res_crude = future_map(data, dd_lr_nonact_int), 
      res_elast = future_map(data, dd_select_nonact_int), 
      data = NULL
    )
  toc()
  
  write_rds(
    res_nonact_int,
    paste0(
      "tables//res_nonact_sim_", i , "_",
      dd_timestamp(),
      ".rds")
  )
  
}

# Combine files
write_rds(
  paste0("tables//", list.files("tables", "res_nonact_sim_[0-9]")) %>%
    map_df(read_rds),
  paste0(
    "tables//res_nonact_sim_",
    n_sim*n_files, 
    "_",
    dd_timestamp(),
    ".rds")
)
