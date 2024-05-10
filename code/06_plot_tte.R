library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)

# load posteriors
lambdas = readRDS(file = "posteriors/lambdas.rds") %>%  
  mutate(heat= case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  rename(lambda = .epred) %>% 
  ungroup %>% 
  select(heat, fish, .draw, lambda)

ppmr = readRDS("posteriors/ppmr_posts.rds") %>%  
  mutate(heat = case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  rename(ppmr = .epred) %>% 
  ungroup %>% 
  select(heat, .draw, fish, ppmr)

scaling_posts = readRDS(file = "posteriors/scaling_posts.rds") %>% select(heat, fish, .draw, value) %>%  
  mutate(heat = case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  rename(gamma = value)

tte = lambdas %>% left_join(scaling_posts) %>% 
  right_join(ppmr) %>% 
  mutate(subsidy = 0.4) %>% 
  mutate(log10_te = log10(ppmr)*(lambda + 1 + gamma),
         te = 10^log10_te)


tte %>% 
  ggplot(aes(x = te, fill = heat)) + 
  stat_halfeye() +
  scale_x_log10()
