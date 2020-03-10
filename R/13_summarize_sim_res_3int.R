
# Summarize simulation results of 3 active interactions -------------------

source("R//01_load_pkgs.R")

# Folders
# Folder to save output tables in and read in simulation results from
folder_to_use <- "" 
# For example
folder_to_use <- "/Users/David/Dropbox/dd_ident_simulation_results/"

# Read data

# From hard drive

res_int <-
  paste0(folder_to_use, list.files(folder_to_use, "3act")) %>%
    read_rds() %>%
    mutate(n_subj = 4500) %>%
    select(n_subj, everything())

# From Dropbox (change dl=0 to dl=1 when copying link)
# These results are presented in the manuscript

url5 <- "https://www.dropbox.com/s/tt4wftf2wtmwayw/res_3act_sim_n4500_comb10K_20200309144003.rds?dl=1" %>% url()

res_int <-
  url5 %>%
  read_rds() %>%
  mutate(n_subj = 4500) %>%
  select(n_subj, everything())

# Check structure
res_int %>% 
  sample_n(10) %>% 
  unnest(res_elast) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    OR_int,
    correlation, 
    cell) %>% 
  summarise(
    # number of simulations with cell-value mentioned at least one
    any_n = sum(n >= 1), 
    # number of simulations with cell-value mentioned at least twice
    n_2_or_more = sum(n >= 2), 
    # number of simulations with cell-value mentioned at least x3
    n_3_or_more = sum(n >= 3), 
    # # sum of simulations with cell-value mentioned
    total_n = sum(n)
  ) %>% 
  ungroup() 

res_int %>% 
  group_by(
    log_odd_limit,
    n_subj, prev2, 
    # OR_main, 
    OR_int, correlation
  ) %>% 
  count() %>% 
  ungroup() %>% 
  summary() # Check how many simulations 

n_sim <- 10000 # Modify according to analyses

# Unnest results

res_elast_t <- # elastic net (with Bonferroni correction)
  res_int %>% 
  unnest(res_elast) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    # OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  summarise(
    # number of simulations with cell-value mentioned at least one
    any_n = sum(n >= 1), 
    # number of simulations with cell-value mentioned at least twice
    n_2_or_more = sum(n >= 2), 
    # number of simulations with cell-value mentioned at least x3
    n_3_or_more = sum(n >= 3), 
    # # sum of simulations with cell-value mentioned
    total_n = sum(n)) %>% 
  ungroup() 

res_simtable <- 
  res_elast_t %>% 
  mutate(analysis = "elast") %>% 
  mutate(
    mean_identified = total_n/n_sim,
    percent = any_n/n_sim*100,
    percent_2 = n_2_or_more/n_sim*100,
    percent_3 = n_3_or_more/n_sim*100
  ) %>% 
  select(-total_n, 
         -any_n, 
         -n_2_or_more, 
         -n_3_or_more) %>% 
  arrange(-OR_int) %>% 
  mutate(
    cell_analysis = paste0(
      cell, "_", analysis)
  )

# write.xlsx(
#   res_simtable,
#   file.path(
#     folder_to_use,
#     paste0("simtable_3act_int_",
#            n_sim, 
#            "_",
#            dd_timestamp(),
#            ".xlsx"))
# )


# Detected at least once -------------------------------------------------

res_simtable_gather_1 <- # At least one simulation
  res_simtable %>% 
  select(
    -cell, -analysis, 
    -percent_2,
    -percent_3,
    -mean_identified 
  ) %>% 
  spread(
    cell_analysis, 
    percent
  ) %>% 
  arrange(
    n_subj,
    prev2,
    OR_int,
    correlation) %>% 
  select(
    n_subj, 
    prev2, 
    OR_int, 
    correlation,
    FP_main_elast,
    TP_int_elast, 
    FP_int_elast, 
    everything()
  )

# Check table with same layout as in manuscript
t1 <- 
  res_simtable_gather_1 %>% 
  transmute(
    n_subj,
    prev2,
    OR_int,
    correlation,
    FP_main = FP_main_elast, 
    FP_int = FP_int_elast, 
    TP_int_x1 = TP_int_elast)


# Detected at least twice ------------------------------------------------

res_simtable_gather_2 <- 
  res_simtable %>% 
  select(
    -cell, -analysis, 
    -percent,
    -percent_3,
    -mean_identified 
  ) %>% 
  spread(
    cell_analysis, 
    percent_2
  ) %>% 
  arrange(
    # OR_main, 
    n_subj,
    prev2,
    OR_int,
    correlation) %>% 
  select(
    n_subj, 
    prev2, 
    OR_int, 
    correlation,
    FP_main_elast,
    TP_int_elast, 
    FP_int_elast, 
    everything()
  )

# Check table with same layout as in manuscript
t2 <- 
  res_simtable_gather_2 %>% 
  transmute(
    n_subj,
    prev2,
    OR_int,
    correlation,
    TPint_x2 = TP_int_elast
    )

# Detected at least three times -------------------------------------------

res_simtable_gather_3 <- 
  res_simtable %>% 
  select(
    -cell, -analysis, 
    -percent,
    -percent_2,
    -mean_identified 
  ) %>% 
  spread(
    cell_analysis, 
    percent_3
  ) %>% 
  arrange(
    n_subj,
    prev2,
    OR_int,
    correlation) %>% 
  select(
    n_subj, 
    prev2, 
    OR_int, 
    correlation,
    FP_main_elast,
    TP_int_elast, 
    FP_int_elast, 
    everything()
  )

# Check table with same layout as in manuscript
t3 <- 
  res_simtable_gather_3 %>% 
  transmute(
    n_subj,
    prev2,
    OR_int,
    correlation,
    TPint_x3 = TP_int_elast
  )


# Table for supplement ----------------------------------------------------

t_full <- 
  full_join(
    t1, t2 
  ) %>% 
  full_join(t3) 

t_full

# write.xlsx(
#   t_full,
#   file.path(
#     paste0(
#       folder_to_use,
#       "simtable_gather_3act_int_",
#       n_sim, 
#       "_",
#     dd_timestamp(),
#     ".xlsx")
#   )
# )
