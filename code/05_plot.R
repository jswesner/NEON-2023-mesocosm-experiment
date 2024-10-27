library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
theme_set(theme_default())

fit_isd = readRDS(file = "models/fit_isd.rds")
dat = fit_isd$data

post_dots = fit_isd$data %>% select(-dw_mg, -counts) %>% 
  distinct() %>% 
  mutate(counts = 1) %>% 
  mutate(temp = case_when(heat == "no heated" ~ "ambient",
                          TRUE ~ "heated")) %>% 
  add_epred_draws(fit_isd, re_formula = NULL)

saveRDS(post_dots, file = "posteriors/post_dots.rds")

post_lines = fit_isd$data %>% select(-dw_mg, -counts, -tank, -xmin, -xmax) %>% 
  distinct() %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  mutate(temp = case_when(heat == "no heated" ~ "ambient",
                          TRUE ~ "heated")) %>% 
  add_epred_draws(fit_isd, re_formula = NA)

plot_2023 = post_dots %>% 
  ggplot(aes(x = temp, y = .epred)) +
  stat_halfeye(data = post_lines, color = "red") +
  stat_pointinterval(aes(group = tank), size = 0.01,
                     .width = 0.95,
                     position = position_jitter(width = 0.03, height = 0)) +
  facet_wrap(~fish) +
  labs(y = "\u03bb",
       x = "",
       title = "2023 Mesocosm Experiment",
       caption = "Model is fitted to power law portion of the data
       after culling using Clauset et al. (2009).") +
  ylim(NA, -1)

ggsave(plot_2023, file = "plots/plot_2023.jpg", width = 6.5, height = 5)

post_dots %>% 
  group_by(tank) %>% 
  median_qi(.epred) %>% 
  arrange(.epred) %>% 
  left_join(dat %>% group_by(tank, heat, fish) %>% tally) %>% 
  ggplot(aes(x = n, y = .epred, color = interaction(heat, fish))) +
  geom_point(aes(size = n)) +
  geom_label(aes(label = tank),
             nudge_y = 0.1) +
  NULL


# simulated body sizes ----------------------------------------------------



