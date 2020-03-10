
# source("R//01_load_pkgs.R")
# paste0(
#   "R/", 
#   list.files("R/", "^0[2-3]_")) %>% 
#   walk(source)

# Function for plotting the prevalence of predictors ----------------------

dd_plot_prop <- function(dt = data) {
  
  # Preprocess data
  proc_data <- dt %>% dd_preprocess() 
  
  prop <- 
    proc_data$exp_count %>% 
    gather(key, value, -predictor, -include) %>% 
    separate(
      key,
      sep = "_y",
      into = c("key", "Status"),
      remove = FALSE
    ) %>% 
    filter(
      key == "prev", 
      include == TRUE
    ) %>% 
    transmute(
      Predictor = predictor %>% str_replace_all("_", " "), 
      Status = if_else(Status == 1, "Case", "Control"), 
      Percentage = value
    )
  
  positions <- 
    prop[
      order(prop[prop$Status == "Control",]$Percentage, 
            decreasing = FALSE),]$Predictor %>% 
    as.character()
  
  plot <- ggplot(prop, aes(x = Predictor, 
                           y = Percentage, 
                           fill = Status)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    coord_flip() +
    scale_x_discrete(limits = positions, name = "Predictor") +
    scale_y_continuous(name = "%") +
    scale_fill_manual(values=c("red", "navy")) +
    theme_light()
  
  return(plot)
  
}

# Test --------------------------------------------------------------------

# dd_plot_prop(dt = dd_sim(seed = 1))

