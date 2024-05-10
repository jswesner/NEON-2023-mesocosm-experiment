
### This code equires an installation of rstan first. visit www.mc-stan.org first and follow instructions. Then install the packages below and 
### run code.
library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
library(ggthemes)
theme_set(theme_default())

# 1) get data
metab = read_csv(file = "data/metabolism.csv")


# 2) fit model
# brm_metab = brm(log_resp_c ~ log_dw_c*heat*fish + (1|tank),
#                 data = metab,
#                 prior = c(prior(normal(0.75, 0.2), coef = "log_dw_c"),
#                           prior(normal(0, 1), class = "b"),
#                           prior(normal(0, 0.2), class = "Intercept")),
#                 file = "models/brm_metab.rds",
#                 file_refit = "on_change",
#                 cores = 4)

brm_metab = readRDS("models/brm_metab.rds")

cond_plot = plot(conditional_effects(brm_metab, effect = "log_dw_c:fish", 
                                conditions = tibble(heat = c("heated", "no heated"))))


cond_plot$`log_dw_c:fish`$data %>% as_tibble() %>% 
  ggplot(aes(x = log_dw_c, y = log_resp_c))



# 3) get metab slopes

post_slopes = brm_metab$data %>% 
  distinct(heat, fish) %>% 
  mutate(tank = 1) %>% # placeholder. Value doesn't matter since re is not used to calculate slope
  expand_grid(log_dw_c = c(0, 1)) %>% 
  add_epred_draws(brm_metab, re_formula = NA) %>% 
  ungroup %>% 
  select(heat, fish, .draw, .epred, log_dw_c) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1` - `0`) %>% 
  mutate(heat = case_when(heat == "no heated" ~ "not heated",
                          TRUE ~ heat),
         heat = fct_relevel(heat, "not heated"))

# 4) plot model
metab_slope_interaction = post_slopes %>% 
  ggplot(aes(y = slope, x = heat, color = fish)) + 
  stat_pointinterval(aes(color = fish)) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  geom_line(data = post_slopes %>% filter(.draw <= 1000),
            aes(group = interaction(fish, .draw)),
            alpha = 0.08,
            linewidth = 0.2)

# save plot
ggsave(metab_slope_interaction, file = "plots/metab_slope_interaction.jpg",
       dpi = 400, width = 6, height = 3)


# mean predicted slope ----------------------------------------------------

predicted_slopes = brm_metab$data %>% 
  distinct(heat, fish) %>% 
  mutate(tank = 1) %>% # placeholder. Value doesn't matter since re is not used to calculate slope
  expand_grid(log_dw_c = c(0,1)) %>% 
  add_epred_draws(brm_metab, re_formula = NULL, allow_new_levels = T) %>% 
  ungroup %>% 
  select(heat, fish, .epred, .draw, log_dw_c) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1` - `0`)

conditional_effects(brm_metab, effects = "log_dw_c", method = "predict",
                    re_formula = NULL)

predicted_slopes %>% 
  reframe(mean = mean(slope),
          sd = sd(slope))




