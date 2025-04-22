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

fit_isd = update(fit_isd, newdata = dw)

saveRDS(fit_isd, file = "models/fit_isd.rds")

