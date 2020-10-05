#' ---
#' title: "Test"
#' date: 2020-10-01
#' author: Taavi Päll
#' output: html_document
#' ---

#' ## R Markdown
  
#' This is an R Markdown document. Markdown is a simple 
#' formatting syntax for authoring HTML, PDF, and MS Word documents. 
#' For more details on using R Markdown see 
#' <http://rmarkdown.rstudio.com>.  
#' 

library(tidyverse)
library("skimr")

hip_fracture <- read_delim("data/hip_fracture.csv", 
                           ";", escape_double = FALSE, col_types = cols(id = col_character()), 
                           trim_ws = TRUE)

skim(hip_fracture)

#' Divide total PT by the length of stay
#' 
mod <- hip_fracture %>% 
  mutate(PT_i = (postacute_therapy / postacute_LOS) * 7)

hist(mod$PT_i)  

mod <- mod %>% 
  mutate(PT_i = case_when(
    is.infinite(PT_i) ~ NA_real_,
    is.nan(PT_i) ~ NA_real_,
    TRUE ~ PT_i
  )) %>% 
  filter(PT_i > 0)


mod %>% 
  ggplot(aes(x = postacute_therapy, linetype = sex)) +
  geom_density()+
  geom_vline(xintercept = mean(mod$postacute_therapy), color = "red") +
  scale_x_log10()

  
geo_mean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm =  na.rm) / length(x))
}

geo_mean(c(0.005, 40000))


mult_SD <- function(x) {
  log_sd <- exp(sd(log(x)))
  log_mean <- geo_mean(x)
  lower <- log_mean / log_sd
  upper <- log_mean * log_sd
  c(geometric_mean = log_mean, lower = lower, upper = upper)
}

mult_SD(mod$postacute_therapy)


with_county <- mod %>% 
  filter(county != "homeless")

with_county %>% 
  ggplot(aes(postacute_therapy)) +
  geom_density() +
  facet_wrap(~ county) +
  scale_x_log10()


table(hip_fracture$postacute_therapy_binned)

summary_county <- hip_fracture %>% 
  mutate(postacute_therapy_binary = case_when(
    postacute_therapy_binned == "Yes" ~ 1,
    TRUE ~ 0
  )) %>% 
  select(county, starts_with("postacute_therapy")) %>% 
  group_by(county) %>% 
  summarise(PT = geo_mean(postacute_therapy),
            frac = mean(postacute_therapy_binary))


summary_county %>% 
  ggplot() +
  geom_point(aes(frac, PT)) +
  facet_wrap(~ county)


summary_county_year <- hip_fracture %>% 
  mutate(postacute_therapy_binary = case_when(
    postacute_therapy_binned == "Yes" ~ 1,
    TRUE ~ 0
  )) %>% 
  select(county, year, starts_with("postacute_therapy")) %>% 
  group_by(county, year) %>% 
  summarise(PT = geo_mean(postacute_therapy),
            frac = mean(postacute_therapy_binary))

summary_county_year %>% 
  ggplot() +
  geom_point(aes(year, frac)) +
  facet_wrap(~ county)
