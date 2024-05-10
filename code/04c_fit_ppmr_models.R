library(tidyverse)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

fish_lengths <- read_csv("data/fish_lengths.csv") %>% 
  mutate(tl_mm = tl_cm*10,
         ww_g = a*tl_cm^b,
         dw_g = ww_g*0.2) %>% 
  left_join(treatments)

fish_ppmr = read_csv("data/prey_size_data.csv") %>% 
  left_join(fish_lengths %>% filter(date != "5/15/2023") %>% distinct(tank, dw_g, heat, fish, treatment)) %>% 
  mutate(fish_dw_mg = dw_g*1000,
         ppmr_raw = fish_dw_mg/dm_mg,
         mean_ppmr = mean(ppmr_raw),
         ppmr_s = ppmr_raw/mean(ppmr_raw))


fish_ppmr %>% 
  ggplot(aes(x = tank, y = ppmr_s,
             color = interaction(heat, fish))) + 
  geom_point() 

# brm_ppmr = brm(ppmr_s ~ heat + (1|tank),
#                data = fish_ppmr,
#                family = Gamma(link = "log"),
#                iter = 2000, chains = 4)
# 
# 
# saveRDS(brm_ppmr, file = "models/brm_ppmr.rds")

brm_ppmr = readRDS(file = "models/brm_ppmr.rds")

ppmr_posts = fish_ppmr %>% 
  distinct(heat, mean_ppmr, tank) %>% 
  add_epred_draws(brm_ppmr, re_formula = NULL) %>%
  mutate(.epred = .epred*mean_ppmr) %>% 
  group_by(heat, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(fish = "fish")

saveRDS(ppmr_posts, file = "posteriors/ppmr_posts.rds")


