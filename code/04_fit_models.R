library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
source("https://raw.githubusercontent.com/jswesner/isdbayes/master/R/paretocounts.R") # loads the models necessary for fitting the isd

# model isd ---------------------------------------------------------------

dw = read_csv("data/dw_raw.csv")

fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ heat*fish, 
              data = dw,
              stanvars = stanvars,  # new thing added by the package
              family = paretocounts(), # new thing added by the package
              chains = 1, iter = 1000,
              file = "models/brm_isd.rds",
              prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                        prior(normal(0, 0.1), class = "b")))



# check model -------------------------------------------------------------

summary(fit)

conditional_effects(fit)
