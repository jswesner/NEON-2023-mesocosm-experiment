library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)
library(viridis)

treatments = read_csv("data/treatments.csv") 

tank_temperature = read_csv("data/tank_temperature.csv") %>% 
  separate(date, into = c("day", "month", "year")) %>% 
  mutate(date = as.Date(paste0(year,"-", month, "-", day))) %>% 
  clean_names() %>% 
  left_join(treatments)
  
temp_plot = tank_temperature %>% 
  ggplot(aes(x = date, y = temperature_c, group = tank, color = treatment)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.2) +
  # facet_wrap(~treatment) +
  scale_color_viridis_d(begin = 0, end = 1) + 
  labs(y = "Water Temperature \u00b0C",
       x = "",
       color = "Treatment")

ggsave(temp_plot, file = "plots/temp_plot.jpg", width = 6.5, height = 4)  
