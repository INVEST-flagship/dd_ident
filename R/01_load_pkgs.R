# Install pacman for easy loading of packages
suppressMessages(
  if(!require(pacman)) install.packages("pacman")
)

# Load packages
p_load(
  tidyverse, 
  simstudy,
  stringr, 
  glmnet, 
  furrr, 
  tictoc, 
  openxlsx, 
  knitr
       )

# Utils function
dd_timestamp <- function() Sys.time() %>% str_replace_all(":| |-", "")