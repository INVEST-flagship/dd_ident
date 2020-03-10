
source("R//01_load_pkgs.R")
paste0(
  "R/",
  list.files("R/", "^0[2-7]_")) %>%
  walk(source)

# Script for summarizing simulation results -------------------------------

# Folders
# Folder to save output tables in and read in simulation results from
folder_to_use <- "" 
# For example
folder_to_use <- "/Users/David/Dropbox/dd_ident_simulation_results/"

# Read data

# From hard drive
res_int <- bind_rows(
  paste0(folder_to_use, list.files(folder_to_use, "res_sim_auc_n4500")) %>%
    map_df(read_rds) %>%
    mutate(n_subj = 4500) %>%
    select(n_subj, everything()),
  paste0(folder_to_use, list.files(folder_to_use, "res_sim_auc_n9K")) %>%
    map_df(read_rds) %>%
    mutate(n_subj = 9000) %>%
    select(n_subj, everything()),
  paste0(folder_to_use, list.files(folder_to_use, "res_sim_auc_n15K")) %>%
    map_df(read_rds) %>%
    mutate(n_subj = 15000) %>%
    select(n_subj, everything())
)

# Online from Dropbox (change dl=0 to dl=1 when copying link)
# These results are presented in the manuscript
url1 <- "https://www.dropbox.com/s/jg1dbbyrb04a3nb/res_sim_auc_n4500_comb10K_20190503215503.rds?dl=1" %>% url()
url2 <- "https://www.dropbox.com/s/nijqq7vlaeb16u6/res_sim_auc_n4500_comb10K_20200309154248.rds?dl=1" %>% url()
url3 <- "https://www.dropbox.com/s/a3wn7si4jjaw6r8/res_sim_auc_n9K_comb10K_20190502150504.rds?dl=1" %>% url()
url4 <- "https://www.dropbox.com/s/zpkqrjovo17gd98/res_sim_auc_n15K_comb10K_20190502040223.rds?dl=1" %>% url()


res_int <- bind_rows(
  url1 %>%
    read_rds() %>%
    mutate(n_subj = 4500) %>%
    select(n_subj, everything()),
  url2 %>%
    read_rds() %>%
    mutate(n_subj = 4500) %>%
    select(n_subj, everything()),
  url3 %>%
    read_rds() %>%
    mutate(n_subj = 9000) %>%
    select(n_subj, everything()),
  url4 %>%
    read_rds() %>%
    mutate(n_subj = 15000) %>%
    select(n_subj, everything())
)


# Extract unnested results ------------------------------------------------

# Check structure
res_int %>% 
  sample_n(10) %>% # For some rows
  unnest(res_elast) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  summarise(
    # number of simulations with cell-value mentioned at least once
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
    n_subj, prev2, OR_main, OR_int, correlation
  ) %>% 
  count() %>% 
  ungroup() %>% 
  summary() # Check how many simulations 

n_sim <- 10000 # Modify according to analyses

# Unnest results

res_crude_nb_t <- # nb = not Bonferroni-corrected marginal screening
  res_int %>% 
  unnest(res_crude_nb) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  summarise(any_n = sum(n >= 1), total_n = sum(n)) %>% 
  ungroup()


res_crude_t <- # marginal screening (with Bonferroni correction)
  res_int %>% 
  unnest(res_crude) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  summarise(any_n = sum(n >= 1), total_n = sum(n)) %>% 
  ungroup() 


res_elast_t <- # elastic net (with Bonferroni correction)
  res_int %>% 
  unnest(res_elast) %>% 
  group_by(
    log_odd_limit,
    n_subj, 
    prev2,
    OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  summarise(any_n = sum(n >= 1), total_n = sum(n)) %>% 
  ungroup() 

res_simtable <- 
  bind_rows(
    res_crude_nb_t %>% 
      mutate(analysis = "crude_nb"), 
    res_crude_t %>% 
      mutate(analysis = "crude"), 
    res_elast_t %>% 
      mutate(analysis = "elast")
  ) %>% 
  mutate(
    mean_identified = total_n/n_sim,
    percent = any_n/n_sim*100
  ) %>% 
  select(-total_n, -any_n) %>% 
  arrange(-OR_int) %>% 
  mutate(
    cell_analysis = paste0(
      cell, "_", analysis)
  )


# write.xlsx(
#   res_simtable,
#   paste0(
#     file.path(
#     folder_to_use,
#     "simtable_"
#     ),
#     dd_timestamp(),
#     ".xlsx")
#   )

res_simtable_gather <- 
  res_simtable %>% 
  select(
    -cell, -analysis, 
    -mean_identified 
  ) %>% 
  spread(
    cell_analysis, 
    percent
  ) %>% 
  arrange(OR_main, 
          n_subj,
          prev2,
          OR_int,
          correlation) %>% 
  select(
    log_odd_limit,
    
    n_subj, prev2, OR_int, OR_main, correlation,
    
    TP_main_elast, FP_main_elast, 
    TP_int_elast, FP_int_elast, 
    
    TP_main_crude, FP_main_crude, 
    TP_int_crude, FP_int_crude, 
    
    TP_main_crude_nb, FP_main_crude_nb, 
    TP_int_crude_nb, FP_int_crude_nb, 
    
    everything()
  )

# Check table with same layout as in manuscript
res_simtable_gather %>% 
  select(
    n_subj,
    prev2,
    OR_int,
    correlation,
    TP_main_elast, FP_main_elast, TP_int_elast, FP_int_elast)


# write.xlsx(
#   res_simtable_gather %>%
#     select(
#       n_subj,
#       prev2,
#       OR_int,
#       correlation,
# 
#       TP_main_elast, FP_main_elast,
#       TP_int_elast, FP_int_elast,
# 
#       TP_main_crude, FP_main_crude,
#       TP_int_crude, FP_int_crude,
# 
#       TP_main_crude_nb, FP_main_crude_nb,
#       TP_int_crude_nb, FP_int_crude_nb
#       ),
#   paste0(
#     file.path(
#       folder_to_use,
#       "simtable_gather_"
#     ),
#     dd_timestamp(),
#     ".xlsx")
# )

