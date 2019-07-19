
# source("R//01_load_pkgs.R")
# paste0(
#   "R/",
#   list.files("R/", "[2-7]_")) %>%
#   walk(source)

# Script for summarizing simulation results -------------------------------

# list.files("tables", "res_")
# list.files("tables", "res_sim_")
# list.files("tables", "res_sim_[0-9]")
# list.files("tables", "res_nonact_")
# list.files("tables", "res_n[0-9]")


# Read data

res_int <- bind_rows(
  paste0("tables//", list.files("tables", "res_sim_[0-9]")) %>% 
    map_df(read_rds) %>% 
    mutate(n = 4500) %>% 
    select(n, everything()),
  paste0("tables//", list.files("tables", "res_nonact_sim_")) %>% 
    map_df(read_rds) %>% 
    mutate(n = 4500) %>% 
    select(n, everything())
)

n_sim <- #

# Extract unnested results ------------------------------------------------

# Check structure
res_int %>% 
  unnest(res_elast) %>% 
  group_by(
    # alpha, 
    log_odd_limit,
    n, 
    prev2,
    OR_main, 
    OR_int,
    correlation, 
    cell) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  rename(any_n = nn)

res_int %>% 
  group_by(
    # alpha, 
    log_odd_limit,
    n, prev2, OR_main, OR_int, correlation
  ) %>% 
  count() %>% 
  ungroup() %>% 
  summary() # Check that everything is run 1000 times

# Unnest results

res_crude_nb_t <- # nb = not Bonferroni-corrected crude results
  full_join(
    
    res_int %>% 
      unnest(res_crude_nb) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      count() %>% 
      ungroup() %>% 
      rename(any_n = nn), 
    
    res_int %>% 
      unnest(res_crude_nb) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      summarise(total_n = sum(n)) %>% #, mean_n = mean(n), sd_n = sd(n)
      ungroup()
    
  )

res_crude_t <- # crude results (with Bonferroni correction)
  full_join(
    
    res_int %>% 
      unnest(res_crude) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      count() %>% 
      ungroup() %>% 
      rename(any_n = nn), 
    
    res_int %>% 
      unnest(res_crude) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      summarise(total_n = sum(n)) %>% 
      ungroup() 
  )

res_elast_t <- # elastic net
  full_join(
    res_int %>% 
      unnest(res_elast) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      count() %>% 
      ungroup() %>% 
      rename(any_n = nn) , 
    
    res_int %>% 
      unnest(res_elast) %>% 
      group_by(
        alpha, 
        log_odd_limit,
        n, 
        prev2,
        OR_main, 
        OR_int,
        correlation, 
        cell) %>% 
      summarise(total_n = sum(n)) %>% 
      ungroup() 
  )

res_simtable <- 
  bind_rows(
  res_crude_nb_t %>% 
    mutate(analysis = "crude_nb"), 
  res_crude_t %>% 
    mutate(analysis = "crude"), 
  res_elast_t %>% 
    mutate(analysis = "elast")
) %>% 
  # filter(
  #   str_detect(cell, "P_")
  # ) %>% 
  mutate(
    mean_identified = total_n/n_sim,
    percent = any_n/n_sim*100,
    percent_se = sqrt(percent/100 * (1 - percent/100) / n_sim), 
    percent_low = percent - 1.96 * percent_se, 
    percent_hi = percent + 1.96 * percent_se
    ) %>% 
  select(-total_n, -any_n) %>% 
  arrange(-OR_int) %>% 
  mutate(
    cell_analysis = paste0(
      cell, "_", analysis), 
    OR_both = paste0("OR_main=", OR_main, ", OR_int=", OR_int), 
    sim_scen = paste0(
      "n=", n, 
      ",m=", OR_main, 
      ",i=", OR_int, 
      ",c=", correlation), 
    Analysis = case_when(
      analysis == "crude_nb" ~ "Non-corrected crude analysis", 
      analysis == "crude" ~ "Bonferroni-corrected crude analysis", 
      analysis == "elast" ~ "Bonferroni-corrected elastic net analysis"
    ), 
    cell_label = case_when(
      cell == "FP_int" ~ "False positive interaction effects", 
      cell == "TP_int" ~ "True positive interaction effects", 
      cell == "FP_main" ~ "At least one false positive main effects", 
      cell == "TP_main" ~ "At least one true positive main effects", 
      TRUE ~ cell
    )
  )

# write.xlsx(res_simtable, "tables//simtable.xlsx")

res_simtable_gather <- 
  res_simtable %>% 
  select(
    -percent_se, -percent_low, -percent_hi,
    -cell, -analysis, 
    -mean_identified, 
    -OR_both, -Analysis, 
    -sim_scen,
    -cell_label
    ) %>% 
  spread(
    cell_analysis, 
    percent
  ) %>% 
  arrange(OR_main, -OR_int, prev2) %>% 
  select(
    alpha, log_odd_limit,
    
    n, prev2, OR_int, OR_main, correlation,
    
    TP_main_elast, FP_main_elast, 
    TP_int_elast, FP_int_elast, 
    
    TP_main_crude, FP_main_crude, 
    TP_int_crude, FP_int_crude, 
    
    TP_main_crude_nb, FP_main_crude_nb, 
    TP_int_crude_nb, FP_int_crude_nb, 
    
    everything()
   )

# write.xlsx(res_simtable_gather, "tables//simtable_gather.xlsx")

# Table for manuscript ----------------------------------------------------

ms_t <- 
  res_simtable_gather %>% 
  filter(
    OR_main == 1.3, 
    n <= 15000
  ) %>% 
  select(
    1:(11+4), 
    -OR_main
  ) %>% 
  arrange(
    n, 
    OR_int, 
    prev2, 
    correlation
  )

ms_t

ms_t <- 
  ms_t[
    c(
      4, 
      8, 9,
      5, 6, 
      1, 7, 
      2, 3
    ), ] 

ms_t <- 
  ms_t[
    c(
      3, 
      2, 4,
      1, 5
    ), ] 


# write.xlsx(ms_t, "tables//table1_10Ksim.xlsx")
# write.xlsx(ms_t, "tables//table_suppl_10Ksim.xlsx")
