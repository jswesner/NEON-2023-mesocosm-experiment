library(tidyverse)
library(tidybayes)
library(brms) 
library(isdbayes)
# Metabolic Scaling posterior predictive ---------------------------------------------------------------

brm_metab = readRDS("models/brm_metab.rds")

# check model 
library(bayesplot)

preds_metab = brm_metab %>%
  posterior_predict(draws = 10) # for some reason this draws still returns 500 draws so we filter in the next step before plotting

dens_trt_metab = preds_metab[1:10,] %>% 
  ppc_dens_overlay_grouped(y = brm_metab$data$log_resp_c,
                           group = interaction(brm_metab$data$heat, brm_metab$data$fish)) +
  scale_x_log10()

# rename treatments then plot
dens_trt_metab$data = dens_trt_metab$data %>% 
  mutate(group = case_when(group == "heated.fish" ~ "d) +heat|+fish",
                           group == "heated.no fish" ~ "c) +heat|-fish",
                           group == "no heated.fish" ~ "b) -heat|+fish",
                           TRUE ~ "a) -heat|-fish"))

dens_trt_metab

ggsave(dens_trt_metab, file = "plots/dens_trt_metab.jpg", width = 5.3, height = 5)

# ISD posterior predictive ---------------------------------------------------------------

dw = readRDS("data/dat_clauset_xmins.rds") %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg))

fit_isd = readRDS("models/fit_isd.rds")



# check model 
library(bayesplot)

preds_isd = fit_isd %>%
  posterior_predict(draws = 10) # for some reason this draws still returns 500 draws so we filter in the next step before plotting

dens_trt_isd = preds_isd[1:10,] %>% 
  ppc_dens_overlay_grouped(y = fit_isd$data$dw_mg,
                   group = interaction(fit_isd$data$heat, fit_isd$data$fish)) +
  scale_x_log10()

# rename treatments then plot
dens_trt_isd$data = dens_trt_isd$data %>% 
  mutate(group = case_when(group == "heated.fish" ~ "d) +heat|+fish",
                           group == "heated.no fish" ~ "c) +heat|-fish",
                           group == "no heated.fish" ~ "b) -heat|+fish",
                           TRUE ~ "a) -heat|-fish"))

dens_trt_isd

ggsave(dens_trt_isd, file = "plots/dens_trt_isd.jpg", width = 5.3, height = 5)









