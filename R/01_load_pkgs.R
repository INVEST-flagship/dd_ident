# Install pacman for easy loading of packages
suppressMessages(
  if(!require(pacman)) {install.packages("pacman");library(pacman)}
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
  reshape2,
  rlang,
  knitr
)

# Utils function
dd_timestamp <- function() Sys.time() %>% str_replace_all(":| |-", "")
