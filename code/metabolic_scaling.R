library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
library(ggthemes)
theme_set(theme_default())

# get data
treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

macro_lw_coeffs <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra/data/raw_data/inverts/macro_lw_coeffs.csv") %>% 
  pivot_longer(cols = c(subphylum, class, order, family, genus, taxon),
               names_to = "group",
               values_to = "taxon") %>% 
  glimpse() %>% 
  group_by(taxon, formula) %>% 
  reframe(a = mean(a),
            b = mean(b)) %>% 
  filter(formula == "M = aL^b") %>% 
  bind_rows(tibble(taxon = "daphnia",   # from Sterner et al https://aslopubs.onlinelibrary.wiley.com/doi/pdf/10.4319/lo.1993.38.4.0857
                   a = -2.7,
                   b = 2.57)) %>% 
  ungroup %>% 
  mutate(taxon = tolower(taxon))

metab = read_delim("data/gjoni_wesner_exp_2023.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  separate(species, into = c("taxon", "stage")) %>% 
  left_join(treatments) %>% 
  left_join(macro_lw_coeffs) %>% 
  ungroup %>% 
  mutate(ww_mg = a*length^b,
         dw_mg = ww_mg*0.2,
         log_dw = log(dw_mg),
         log_resp = log(resp_rate),
         log_dw_c = log_dw - mean(log_dw),
         log_resp_c = log_resp - mean(log_resp))

write_csv(metab, file = "data/metabolism.csv")


metab %>% 
  ggplot(aes(x = log_dw_c, y = log_resp_c)) +
  geom_point(aes(color = treatment)) +
  geom_smooth(method = "lm", aes(fill = treatment)) 

# model
metab = read_csv(file = "data/metabolism.csv")

get_prior(log_resp ~ log_dw*heat*fish,
          data = metab,
          family = gaussian())

# brm_metab = brm(log_resp_c ~ log_dw_c*heat*fish + (1|tank),
#                 data = metab,
#                 prior = c(prior(normal(0.75, 0.2), coef = "log_dw_c"),
#                           prior(normal(0, 1), class = "b"),
#                           prior(normal(0, 0.2), class = "Intercept")),
#                 file = "models/brm_metab.rds",
#                 file_refit = "on_change",
#                 cores = 4)

brm_metab = readRDS("models/brm_metab.rds")

# get metab slopes
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



# plot
metab_slope_interaction = post_slopes %>% 
  ggplot(aes(y = slope, x = heat, color = fish)) + 
  stat_pointinterval(aes(color = fish)) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  geom_line(data = post_slopes %>% filter(.draw <= 1000),
            aes(group = interaction(fish, .draw)),
            alpha = 0.08,
            linewidth = 0.2)

ggsave(metab_slope_interaction, file = "plots/metab_slope_interaction.jpg",
       dpi = 400, width = 6, height = 3)


# treatment comparison ----------------------------------------------------


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

# tank slopes -------------------------------------------------------------

# brm_metab_tankslopes = update(brm_metab, formula = . ~ log_dw_c*heat*fish + (1 + log_dw_c|tank),
#                               newdata = metab)
# 
# saveRDS(brm_metab_tankslopes, file = "models/brm_metab_tankslopes.rds")
brm_metab_tankslopes = readRDS(file = "models/brm_metab_tankslopes.rds")


post_tank_slopes = brm_metab_tankslopes$data %>% 
  distinct(tank, heat, fish) %>% 
  expand_grid(log_dw_c = c(0, 1)) %>% 
  add_epred_draws(brm_metab_tankslopes, re_formula = NULL) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1` - `0`)

tank_slopes = post_tank_slopes %>% 
  group_by(tank, heat, fish) %>% 
  median_qi(slope) %>% 
  mutate(model = "metabolic scaling slopes")
  
write_csv(tank_slopes, file = "tables/tank_slopes.csv")

posts_tankslopes = brm_metab_tankslopes$data %>% 
  distinct(tank, heat, fish) %>% 
  expand_grid(log_dw_c = seq(min(brm_metab_tankslopes$data$log_dw_c),
                             max(brm_metab_tankslopes$data$log_dw_c),
                             length.out = 30)) %>% 
  add_epred_draws(brm_metab_tankslopes, re_formula = NULL,
                  ndraws = 500)

posts_tankslopes %>% 
  ggplot(aes(x = log_dw_c, y = .epred)) + 
  stat_lineribbon(aes(group = tank, fill = heat), .width = 0.95, alpha = 0.4) +
  facet_wrap(~tank) +
  geom_point(data = metab, aes(y = log_resp_c), size = 0.2)

