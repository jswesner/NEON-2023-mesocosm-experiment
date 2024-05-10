library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)
library(readxl)

# model isd ---------------------------------------------------------------

dw <- read_csv("data/dw_fixed.csv") %>% 
  mutate(tank = as.integer(tank),
         tank_f = as.factor(tank))

# TAKES ~40 MINUTES
# brm_isd_rand = update(brm_isd, newdata = dw, 
#                  formula = . ~ heat*fish + (1|tank_f),
#                  prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
#                            prior(normal(0, 0.1), class = "b"),
#                            prior(exponential(6), class = "sd")),
#                  chains = 4, iter = 2000)
# 
# saveRDS(brm_isd_rand, file = "models/brm_isd_rand.rds")

brm_isd_rand = readRDS(file = "models/brm_isd_rand.rds")

  
lambdas = brm_isd_rand$data %>% 
  distinct(heat, fish, xmin, xmax) %>%
  mutate(counts = 1) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NA)

saveRDS(lambdas, file = "posteriors/lambdas.rds")
