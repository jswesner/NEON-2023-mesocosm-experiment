library(tidyverse)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat_plus, "|",fish_plus))

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

fish_growth = fish_lengths %>% 
  mutate(date = mdy(date)) %>% 
  select(treatment, date, dw_g, tank) %>% 
  pivot_wider(names_from = date, values_from = dw_g) %>% 
  mutate(growth = `2023-06-19` - `2023-05-15`) %>% 
  arrange(treatment, tank)

write_csv(fish_growth, file = "tables/fish_growth.csv")


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
