library(tidyverse)
library(tidybayes)
library(brms) 
library(isdbayes)

# load models
brm_metab = readRDS("models/brm_metab_randslope.rds")
fit_isd = readRDS("models/fit_isd.rds")

# sample priors
brm_metab_prior = update(brm_metab, sample_prior = "only")
fit_isd_prior = update(fit_isd, sample_prior = "only")

saveRDS(brm_metab_prior, file = "models/brm_metab_prior.rds")
saveRDS(fit_isd_prior, file = "models/fit_isd_prior.rds")

# plot priors
library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
library(ggthemes)
theme_set(theme_default())


treatments = read_csv("data/treatments.csv") 

fit_isd_prior = readRDS(file = "models/fit_isd_prior.rds")
dat = fit_isd_prior$data

prior_dots = fit_isd_prior$data %>% select(-dw_mg, -counts) %>% 
  distinct() %>% 
  mutate(counts = 1) %>% 
  mutate(temp = case_when(heat == "no heated" ~ "ambient",
                          TRUE ~ "heated")) %>% 
  add_epred_draws(fit_isd_prior, re_formula = NULL)

# saveRDS(prior_dots, file = "posteriors/prior_dots.rds")

prior_lines = fit_isd_prior$data %>% select(-dw_mg, -counts, -tank, -xmin, -xmax) %>% 
  distinct() %>% 
  mutate(counts = 1,
         xmin = min(fit_isd_prior$data$xmin),
         xmax = max(fit_isd_prior$data$xmax)) %>% 
  mutate(temp = case_when(heat == "no heated" ~ "ambient",
                          TRUE ~ "heated")) %>% 
  add_epred_draws(fit_isd_prior, re_formula = NA)

plot_prior_2023 = prior_dots %>% 
  ggplot(aes(x = temp, y = .epred)) +
  stat_halfeye(data = prior_lines, color = "red") +
  stat_pointinterval(aes(group = tank), size = 0.01,
                     .width = 0.95,
                     position = position_jitter(width = 0.03, height = 0)) +
  facet_wrap(~fish) +
  labs(y = "\u03bb",
       x = "") +
  ylim(NA, -1)

ggsave(plot_prior_2023, file = "plots/plot_prior_2023.jpg", width = 6.5, height = 5)


# metabolic scaling -------------------------------------------------------

brm_metab_prior = readRDS("models/brm_metab_prior.rds")

metab_priors = brm_metab_prior$data %>% 
  distinct(heat, fish) %>%
  expand_grid(log_dw_c = c(0, 1)) %>% 
  add_epred_draws(brm_metab_prior, re_formula = NA)

metab_prior_slopes = metab_priors %>% 
  ungroup %>% 
  select(heat, fish, log_dw_c, .draw, .epred) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1` - `0`)


metab_prior_wrangled = metab_prior_slopes %>% 
  select(heat, fish, .draw, slope) %>% 
  rename(value = slope) %>% 
  mutate(measure = "b) Metabolic Scaling Slope")

isd_prior_wrangled = prior_lines %>% 
  ungroup %>% 
  select(heat, fish, .draw, .epred) %>% 
  rename(value = .epred) %>% 
  mutate(measure = "a) ISD \u03bb")

metab_isd_prior = bind_rows(metab_prior_wrangled,
                      isd_prior_wrangled) %>% 
  left_join(treatments %>% distinct(fish, heat, fish_plus, heat_plus, treatment)) %>% 
  mutate(prior_post = "Prior") %>% 
  bind_rows(readRDS(file = "posteriors/metab_isd_posteriors.rds") %>% 
              mutate(prior_post = "Posterior"))

# saveRDS(metab_isd, file = "posteriors/metab_isd_posteriors.rds")

lambda_plot_prior = metab_isd_prior %>% 
  filter(grepl("a)", measure)) %>% 
  ggplot(aes(x = heat_plus, y = value, fill = treatment, alpha = prior_post)) + 
  stat_slab(size = 0.2, normalize = "groups") +
  facet_wrap(~fish_plus) +
  labs(y = "\u03bb",
       subtitle = "b)") +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(-2.5, -1)) +
  # scale_fill_colorblind() +
  scale_fill_viridis_d() +
  scale_alpha_discrete(range = c(0.8, 0.2)) +
  guides(fill = "none",
         alpha = "none")

labels = tibble(fish_plus = "+fish", 
                heat_plus = "+heat",
                value = c(1.5, 0),
                label = c("Prior", "Posterior"),
                treatment = "+heat|+fish",
                prior_post = c("Prior", "Posterior"))

scaling_plot_prior = metab_isd_prior %>% 
  filter(!grepl("a)", measure)) %>% 
  ggplot(aes(x = heat_plus, y = value, fill = treatment)) + 
  stat_slab(size = 0.2, normalize = "groups", aes(alpha = prior_post)) +
  facet_wrap(~fish_plus, scales = "free") +
  labs(y = "Metabolic Scaling Slope",
       subtitle = "a)") +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  scale_fill_viridis_d() +
  scale_alpha_discrete(range = c(0.8, 0.2)) +
  guides(fill = "none",
         alpha = "none") +
  geom_text(data = labels, aes(label = label), size = 2, hjust = 0,
            alpha = c(0.6, 0.8))

library(patchwork)
lambda_scaling_plot_prior = scaling_plot_prior/lambda_plot_prior
ggsave(lambda_scaling_plot_prior, file = "plots/lambda_scaling_plot_prior.jpg", width = 5, height = 6, dpi = 400)


