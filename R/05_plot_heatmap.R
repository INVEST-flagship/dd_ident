
# source("R//01_load_pkgs.R")
# 
# paste0(
#   "R/", 
#   list.files("R/", "[2-4]_")) %>% 
#   walk(source)

# Function for heatmap ----------------------------------------------------

dd_heatmap <-
  function(
    dt = data, 
    name = "log(OR)\n", 
    log_or_limit = 0.3,
    limit = c(-5, 5)) {
    
    proc_data <- dt %>% dd_preprocess()
    proc_data <- proc_data$cor_table
    
    proc_data %>% 
      mutate(
        Var1 = Var1 %>% str_replace_all("_", " "),
        Var2 = Var2 %>% str_replace_all("_", " ")
      )
    
    ggplot(data = proc_data, aes(Var2, Var1, fill = log_odds_ratio))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "navy", high = "red", mid = "white",
                           midpoint = 0, limit = limit, space = "Lab",
                           name=paste(name)) +
      scale_x_discrete(
        breaks = levels(proc_data$Var2),
        labels = levels(proc_data$Var2)) +
      scale_y_discrete(
        breaks = levels(proc_data$Var1),
        labels = levels(proc_data$Var1)) +
      coord_fixed() +
      geom_text(aes(
        Var2, 
        Var1, 
        label = 
          paste0(
            log_odds_ratio %>% round(1), 
            ifelse(
              abs(log_odds_ratio) >= log_or_limit, 
              "*", 
              "") 
          ) %>%
          str_replace_all("NA", "") 
      ), 
      color = "black", 
      size = 4) +
      theme(
        axis.text.x = element_text(angle = 45,
                                   size  = 12,
                                   vjust = 1,
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()
      )
  }


# Test --------------------------------------------------------------------

# dd_heatmap(dt = dd_sim(seed = 1))
