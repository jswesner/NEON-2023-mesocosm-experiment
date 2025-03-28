library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
library(ggthemes)
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
       x = "") +
  ylim(NA, -1)

ggsave(plot_2023, file = "plots/plot_2023.jpg", width = 6.5, height = 5)


# metabolic scaling -------------------------------------------------------

brm_metab = readRDS("models/brm_metab.rds")

metab_posts = brm_metab$data %>% 
  distinct(heat, fish) %>%
  expand_grid(log_dw_c = c(0, 1)) %>% 
  add_epred_draws(brm_metab, re_formula = NA)

metab_slopes = metab_posts %>% 
  ungroup %>% 
  select(heat, fish, log_dw_c, .draw, .epred) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1` - `0`)


metab_wrangled = metab_slopes %>% 
  select(heat, fish, .draw, slope) %>% 
  rename(value = slope) %>% 
  mutate(measure = "b) Metabolic Scaling Slope")

isd_wrangled = post_lines %>% 
  ungroup %>% 
  select(heat, fish, .draw, .epred) %>% 
  rename(value = .epred) %>% 
  mutate(measure = "a) ISD \u03bb")

metab_isd = bind_rows(metab_wrangled,
                      isd_wrangled) %>% 
  mutate(heat = case_when(heat == "heated" ~ "Heated",
                          TRUE ~ "Ambient"),
         fish = case_when(fish == "fish" ~ "Fish",
                          TRUE ~ "No Fish"))

lambda_plot = metab_isd %>% 
  filter(grepl("a)", measure)) %>% 
  ggplot(aes(x = heat, y = value, fill = fish, alpha = heat)) + 
  stat_halfeye(size = 0.2) +
  facet_wrap(~fish) +
  labs(y = "\u03bb",
       subtitle = "a)") +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(-2.5, -1)) +
  scale_fill_colorblind() +
  scale_alpha_discrete(range = c(0.2, 0.8)) +
  guides(fill = "none",
         alpha = "none")


scaling_plot = metab_isd %>% 
  filter(!grepl("a)", measure)) %>% 
  ggplot(aes(x = heat, y = value, fill = fish, alpha = heat)) + 
  stat_halfeye(size = 0.2) +
  facet_wrap(~fish) +
  labs(y = "Metabolic Scaling Slope",
       subtitle = "b)") +
  theme(axis.title.x = element_blank()) +
  scale_fill_colorblind() +
  scale_alpha_discrete(range = c(0.2, 0.8)) +
  guides(fill = "none",
         alpha = "none")


library(patchwork)
lambda_scaling_plot = lambda_plot/scaling_plot
ggsave(lambda_scaling_plot, file = "plots/lambda_scaling_plot.jpg", width = 5, height = 6, dpi = 400)
