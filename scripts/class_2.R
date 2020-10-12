#' ---
#' title: "Class 2"
#' date: "2020-10-07"
#' author: "Taavi & Ülo"
#' ---
#' 
#+ libs
library(tidyverse)
library(here)

#' Loading dataset. Use `?here` for help.
#+ data
hip_fracture <- read_delim(here("data/hip_fracture.csv"), 
                           delim = ";", 
                           escape_double = FALSE, 
                           col_types = cols(id = col_character()), 
                           trim_ws = TRUE)

#' 1. Visualize dementia ~ PT
#' 
#+ 
hip_fracture %>% 
  select(year, sex, age_binned10, county, dementia, postacute_therapy)

#' Let's see how is post-acute therapy related to dementia. 
#' Dementia is here coded as a binary variable 0/1 (it's numeric variable).
#' We want to coerce it to a categorical variable, like characters or booleans. 
#' Now we can get nicer and more reasonable looking plot.

hip_fracture_sel <- hip_fracture %>% 
  filter(county != "homeless") %>% 
  select(year, sex, age_binned10, county, dementia, postacute_therapy, comorbidity, management_method, status_12m)

hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  mutate_at("dementia", as.logical) %>% 
  ggplot(aes(dementia, postacute_therapy)) +
  geom_boxplot() +
  geom_point(position = position_jitter(0.3), size = 0.2, alpha = 0.3) + # to reduce overplotting
  scale_y_log10() +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Dementia", y = "Postacute therapy, h")

hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  mutate_at("dementia", as.logical) %>% 
  ggplot(aes(dementia, postacute_therapy)) +
  geom_violin() +
  scale_y_log10()

#' How does PT depend on physical therapy method or comorbidities?
#' 
hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  ggplot(aes(management_method, postacute_therapy)) +
  geom_boxplot() +
  geom_point(position = position_jitter(0.3), size = 0.2, alpha = 0.3) + # to reduce overplotting
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Management method", y = "Postacute therapy, h")


p <- hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  mutate_at("comorbidity", as.character) %>% 
  ggplot(aes(comorbidity, postacute_therapy)) +
  geom_boxplot() +
  geom_point(position = position_jitter(0.3), size = 0.2, alpha = 0.3) + # to reduce overplotting
  scale_y_log10() +
  scale_x_discrete(labels = as.character(0:11)) +
  labs(x = "Number of comorbidities", y = "Postacute therapy, h")

p +
  facet_wrap(~ county)


hip_fracture_sel_sum <- hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  group_by(county, comorbidity) %>% 
  summarise(postacute_therapy = median(postacute_therapy))

hip_fracture_sel_sum %>% 
  ggplot(aes(comorbidity, postacute_therapy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ county, scales = "free_y")

#' 
#'  
hip_fracture_sel %>% 
  filter(postacute_therapy > 0) %>% 
  mutate(pt_log = log2(postacute_therapy)) %>% 
  group_by()
  

