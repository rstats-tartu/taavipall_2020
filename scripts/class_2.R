
library(tidyverse)
library(here)

hip_fracture <- read_delim(here("data/hip_fracture.csv"), 
                           delim = ";", 
                           escape_double = FALSE, 
                           col_types = cols(id = col_character()), 
                           trim_ws = TRUE)

#' 1. Visualize dementia ~ PT
#' 
hip_fracture %>% 
  select(year, sex, age_binned10, county, dementia, postacute_therapy)

 

