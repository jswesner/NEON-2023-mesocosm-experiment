library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 

dw_raw = read_csv("data/dw_raw.csv")


dw_raw %>% 
  ggplot(aes(x = tank, y = dw_mg)) +
  geom_point() 

dw_raw %>% 
  arrange(-dw_mg)

# shows typo with length = 4282. Remove that one and re-save

dw_fixed = dw_raw %>% 
  filter(length_mm <= 50) %>% 
  filter(length_mm > 0) %>% 
  # group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  write_csv(., file = "data/dw_fixed.csv")

dw_fixed %>% 
  ggplot(aes(x = tank, y = dw_mg, color = treatment)) +
  geom_jitter() +
  scale_y_log10()

dw_fixed %>% 
  ggplot(aes(x = length_mm)) +
  geom_histogram() +
  facet_wrap(~name) +
  scale_x_log10()
