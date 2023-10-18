library(tidyverse)
library(readxl)


treatments <- read_csv("data/treatments.csv")

tank_temperature <- read_csv("data/tank_temperature.csv") %>% 
  mutate(date = dmy(date),
         month = month(date)) %>% 
  left_join(treatments) %>% 
  janitor::clean_names()

tank_temperature %>% 
  filter(month == max(month)) %>% 
  group_by(heat) %>% 
  tidybayes::median_qi(temperature_c)
  
tank_temperature %>% 
  ggplot(aes(x = date, y = temperature_c, color = heat)) +
  geom_point() + 
  geom_line(aes(group = tank))

