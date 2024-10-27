library(tidyverse)
library(tidybayes)
library(brms) 
library(isdbayes)
# model isd ---------------------------------------------------------------

dw = readRDS("data/dat_clauset_xmins.rds") %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg))

fit_isd = readRDS("models/fit_isd.rds")

# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ heat*fish,
#               data = dw,
#               stanvars = stanvars,  # new thing added by the package
#               family = paretocounts(), # new thing added by the package
#               chains = 1, iter = 1000,
#               file = "models/brm_isd.rds",
#               prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
#                         prior(normal(0, 0.1), class = "b")))

fit_isd = update(fit_isd, newdata = dw, formula = . ~ heat*fish + (1|tank),
                 prior = c(prior(normal(-1.6, 0.2), class = 'Intercept'),
                           prior(normal(0, 0.1), class = "b")),
                 iter = 1000, chains = 1)

saveRDS(fit_isd, file = "models/fit_isd.rds")

# check model -------------------------------------------------------------

post_dots  = dw %>% 
  select(heat, fish, tank, xmin, xmax) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(fit_isd)

post_dots %>% 
  group_by(fish, tank, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  ggplot(aes(x = fish, y = .epred)) + 
  stat_pointinterval(aes(group = tank), 
                     position = position_jitter(width = 0.1))

post_predicts = dw %>% 
  # slice(1:10) %>% 
  add_predicted_draws(fit_isd, ndraws = 500)

post_predicts %>% 
  filter(.draw <= 10) %>% 
  ggplot(aes(x = .prediction)) + 
  geom_density(aes(group = .draw)) +
  scale_x_log10() +
  facet_wrap(~tank, scales = "free") +
  geom_density(data = dw, aes(x = dw_mg), color = "dodgerblue")

post_predicts %>% 
  group_by(.draw) %>% 
  reframe(median = median(.prediction),
          gm = exp(mean(log(.prediction)))) %>% 
  ggplot(aes(x = median)) + 
  geom_histogram() + 
  geom_vline(data = dw %>% ungroup %>% reframe(median = median(dw_mg),
                                                   gm = exp(mean(log(dw_mg)))),
             aes(xintercept = median))
