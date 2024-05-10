library(tidyverse)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

fish_lengths <- read_csv("data/fish_lengths.csv") %>% 
  mutate(tl_mm = tl_cm*10,
         ww_g = a*tl_cm^b,
         dw_g = ww_g*0.2) %>% 
  left_join(treatments)


fish_lengths %>% 
  filter(tank != 4) %>% # fish 4 could not be measured accurately at the end due to being mangled in the valve during collection 
  ggplot(aes(x = date, y = dw_g, color = heat)) + 
  geom_point() + 
  geom_line(aes(group = tank, color = heat)) 


fish_lengths %>% 
  select(tank, date, dw_g, heat) %>% 
  pivot_wider(names_from = date, values_from = dw_g) %>% 
  mutate(relative_growth = `6/19/2023`/`5/15/2023`) %>% 
  ggplot(aes(x = heat, y = relative_growth)) + 
  # geom_boxplot(aes(group = heat)) + 
  geom_point()



# fish prey ---------------------------------------------------------------

fish_prey = read_csv("data/prey_size_data.csv") %>% 
  left_join(fish_lengths %>% filter(date != "5/15/2023") %>% distinct(tank, dw_g, heat, fish, treatment)) %>% 
  mutate(fish_dw_mg = dw_g*1000,
         ppmr_raw = fish_dw_mg/dm_mg,
         ppmr_log = round(log10(ppmr_raw)))

fish_prey %>% 
  group_by(tank) %>% 
  reframe(median = median(ppmr_raw)) %>% 
  reframe(median = median(median))
  
fish_prey %>% 
  ggplot(aes(x = tank, y = ppmr_raw, color = heat)) + 
  geom_jitter(width = 0.1) +
  scale_y_log10()


fish_prey %>% 
  group_by(treatment) %>% 
  reframe(dm_mg = median(dm_mg),
          sd = sd(dm_mg)) %>% 
  arrange(treatment, -dm_mg)

fish_prey %>% 
  group_by(treatment) %>%
  median_qi(dm_mg, .width = 0.5) %>% 
  arrange(treatment, dm_mg)


brm_fish_prey = brm(dm_mg ~ treatment,
                    family = Gamma(),
                    data = fish_prey)

saveRDS(brm_fish_prey, file = "models/brm_fish_prey.rds")

brm_fish_prey$data %>% 
  distinct(treatment) %>% 
  add_epred_draws(brm_fish_prey) %>% 
  group_by(treatment) %>% 
  median_qi(.epred)
