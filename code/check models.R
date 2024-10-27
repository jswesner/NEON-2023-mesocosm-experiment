library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
theme_set(theme_default())

fit_isd = readRDS(file = "models/fit_isd.rds")
dat = fit_isd$data

pp_check(fit_isd, type = "stat", stat = "median") +
  scale_x_log10()

pp_check(fit_isd, group = "tank", type = "stat_grouped", stat = "median") +
  scale_x_log10()


dat %>% 
  select(-dw_mg) %>% distinct() %>% 
  add_predicted_draws(fit_isd, .ndraws = 1000) %>% 
  ggplot(aes(x = .prediction, y = tank)) + 
  geom_point(position = position_jitter(width = 2, height = 0),
              size = 0.01) +
  scale_x_log10()

dat %>% 
  select(-dw_mg) %>% distinct() %>%
  add_predicted_draws(fit_isd, .ndraws = 1000) %>%
  ggplot(aes(x = .prediction, y = tank)) + 
  geom_point(position = position_jitter(width = 2, height = 0),
             size = 0.01) +
  scale_x_log10()



