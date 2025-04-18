library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
theme_set(theme_default())

fit_isd = readRDS(file = "models/fit_isd.rds")
dat = fit_isd$data

post_dots = readRDS(file = "posteriors/post_dots.rds")

tank_lambdas = post_dots %>% 
  group_by(tank) %>% 
  median_qi(.epred)

write_csv(tank_lambdas, file = "tables/tank_lambdas.csv")


#iter = 4000, 
#chains = 4,
#threads = 12,
#cores = 32)