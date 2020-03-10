# Define intercept --------------------------------------------------------

#######################################
# NB Computationally extensive
#######################################

# 3 active main effects and 1 active interaction --------------------------

source("R//01_load_pkgs.R")
source("R//02_simdata.R")

# Helper function returning proportion of y=1
dd_intercept <- function(
  n = 1e6,
  prev1 = 0.20,
  prev2 = 0.15,
  prev3 = .05,
  OR_main = 1.3,
  OR_int = 5,
  intercept = -.88,
  correlation = 0.3
) {
  dd_sim(n = n,
         prev1 = prev1,
         prev2 = prev2,
         prev3 = prev3,
         OR_main = OR_main,
         OR_int = OR_int,
         intercept = intercept,
         correlation = correlation) %>%
    count(y) %>%
    mutate(percent = n / sum(n) * 100) %>%
    filter(y == 1) %>%
    select(percent) %>%
    pull()
}

# dd_intercept()

# Scenarios
args_scen <-
  expand.grid(
    n = 1e6,       # Number of subjects for each scenario (large n for stability)
    prev1 = 0.2,   # alternative: seq(0.20, 0.20, 0.05),
    prev2 = 0.15,  # alternative: seq(0.13, 0.2, 0.01),
    prev3 = 0.05,  # alternative: seq(.05, .05, 0.05),
    OR_main = 1.3, # alternative: seq(1.3, 1.3, 1),
    OR_int =  3,   # alternative: seq(1, 5, 2), # seq(1, 5, 2),
    correlation = 0.3 # alternative: c(0.0, 0.1, 0.2, 0.4, 0.5) 
  ) %>%
  as_tibble()

args_scen <-
  args_scen %>%
  mutate(
    scenario = sprintf("%05d", 1:nrow(args_scen))
  )

# Add possible intercepts
args_t <-
  suppressWarnings(
    args_scen %>%
      right_join(
        expand.grid(
          scenario = args_scen %>% pull(scenario),
          intercept = seq(-1.0, -0.7, 0.005)
        ) %>% as_tibble(),
        by = "scenario"
      )
  )

# make list of arguments
args_list <-
  list(
    n = as.list(args_t$n),
    prev2 = as.list(args_t$prev2),
    OR_main = as.list(args_t$OR_main),
    OR_int = as.list(args_t$OR_int),
    intercept = as.list(args_t$intercept),
    correlation = as.list(args_t$correlation)
  )

# Multiprocess
plan(
  multiprocess(
    workers = 3 # Choose manually number of cores
    # availableCores()-1 # Use all available cores except one
  )
)

# Make table with results
res_intercept <-
  args_t %>%
  mutate(
    prop_y = future_pmap_dbl(args_list, dd_intercept), # map: arguments and fun
    diff_33 = abs((1/3)*100-prop_y) # difference to 1/3
  ) %>%
  group_by(scenario) %>%
  mutate(min_33 = min(diff_33)) %>% # difference to 1/3 in each scenario
  ungroup() %>%
  filter(diff_33 == min_33) %>% #select smallest value
  arrange(scenario)

res_intercept %>% summary()

# Write results from
# write_rds(res_intercept,
#           paste0("tables//intercept_",
#                  dd_timestamp(),
#                  ".rds"))


# 3 active interactions but no active main effects ------------------------

source("R//01_load_pkgs.R")
source("R//10_simdata_3int.R")

# Helper function returning proportion of y=1
dd_intercept_3 <- function(
  n = 1e6,
  # prev1 = 0.20,
  prev1 = 0.15,
  prev2 = 0.15,
  prev3 = 0.15,
  # OR_main = 1.3,
  OR_int = 3,
  intercept = -0.95,
  correlation = 0.3
) {
  dd_sim_3(n = n,
           prev1 = prev1,
           prev2 = prev2,
           prev3 = prev3,
           # OR_main = OR_main,
           OR_int = OR_int,
           intercept = intercept,
           correlation = correlation) %>%
    count(y) %>%
    mutate(percent = n / sum(n) * 100) %>%
    filter(y == 1) %>%
    select(percent) %>%
    pull()
}

# Test
# dd_intercept_3(n = 1e5, OR_int = 1.5, intercept = -0.7)

# Scenarios
args_scen <-
  expand.grid(
    n = 1e6,      # Number of subjects for each scenario (large n for stability)
    # prev1 = 0.2,   
    prev1 = 0.15,   
    prev2 = 0.15,  
    prev3 = 0.15,  
    # OR_main = 1.3, 
    OR_int =  seq(1.5, 5, 0.5),   
    correlation = 0.3 
  ) %>%
  as_tibble()

args_scen <-
  args_scen %>%
  mutate(
    scenario = sprintf("%05d", 1:nrow(args_scen))
  )

# Add possible intercepts
args_t <-
  suppressWarnings(
    args_scen %>%
      right_join(
        expand.grid(
          scenario = args_scen %>% pull(scenario),
          intercept = seq(-0.85, -0.7, 0.005)
          # intercept = seq(-1.05, -0.85, 0.005)
        ) %>% as_tibble(),
        by = "scenario"
      )
  )

# make list of arguments
args_list <-
  list(
    n = as.list(args_t$n),
    prev2 = as.list(args_t$prev2),
    # OR_main = as.list(args_t$OR_main),
    OR_int = as.list(args_t$OR_int),
    intercept = as.list(args_t$intercept),
    correlation = as.list(args_t$correlation)
  )

# Multiprocess
plan(
  multiprocess(
    workers = 2 # Choose manually number of cores
    # availableCores()-1 # Use all available cores except one
  )
)

# Make table with results
res_intercept <-
  args_t %>%
  mutate(
    prop_y = future_pmap_dbl(args_list, dd_intercept_3), # map: arguments and fun
    diff_33 = abs((1/3)*100-prop_y) # difference to 1/3
  ) %>%
  group_by(scenario) %>%
  mutate(min_33 = min(diff_33)) %>% # difference to 1/3 in each scenario
  ungroup() %>%
  filter(diff_33 == min_33) %>% #select smallest value
  arrange(scenario)

res_intercept %>% summary()

# Write results from
# write_rds(res_intercept,
#           paste0("tables//intercept_3int_",
#                  dd_timestamp(),
#                  ".rds"))
