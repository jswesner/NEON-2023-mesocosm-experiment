library(tidyverse)
library(ggthemes)
library(janitor)

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

temperature <- read_csv("data/tank_temperature.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy(date),
         month = month(date)) %>% 
  mutate(temp_deg_c = temperature_c) %>% 
  left_join(treatments) %>% 
  mutate(heat_plus = case_when(heat == "heated" ~ "+heat",
                               TRUE ~ "-heat"),
         fish_plus = case_when(fish == "fish" ~ "+fish", TRUE ~ "-fish"),
         fish_heat_plus = paste0(heat_plus, " | ", fish_plus)) 



# summarize ---------------------------------------------------------------
temperature %>% 
  group_by(fish_heat_plus, date) %>% 
  reframe(mean = mean(temp_deg_c),
          sd = sd(temp_deg_c))

temp_table = temperature %>% 
  group_by(fish_heat_plus) %>% 
  reframe(mean = mean(temp_deg_c),
          sd = sd(temp_deg_c))

temperature %>% 
  group_by(heat_plus, date) %>% 
  reframe(mean = mean(temp_deg_c)) %>% 
  pivot_wider(names_from = heat_plus, values_from = mean) %>% 
  group_by(date) %>% 
  mutate(diff = `+heat` - `-heat`) %>%
  ungroup %>% 
  # group_by(fish_plus) %>% 
  reframe(mean = mean(diff),
          sd = sd(diff))

# Plot temperature --------------------------------------------------------
temp_plot = temperature %>% 
  ggplot(aes(x = date, y = temp_deg_c, color = fish_heat_plus)) + 
  geom_point(size = 0.3) + 
  geom_line(aes(group = tank), alpha = 0.5, linewidth = 0.2) +
  scale_color_colorblind() + 
  theme_classic() + 
  # scale_y_log10() +
  labs(y = "Water Temperature (\u00b0C)",
       x = "",
       color = "Treatment") +
  NULL

ggsave(temp_plot, file = "plots/temp_plot.jpg", width = 5, height = 3)
