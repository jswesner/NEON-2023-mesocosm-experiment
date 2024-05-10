library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)
library(janitor)
library(ggview)
library(ggspectra)
# library(LakeMetabolizer)
# library(streamMetabolizer)

# length-weight equations
macro_lw_coeffs <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra/data/raw_data/inverts/macro_lw_coeffs.csv") %>% 
  pivot_longer(cols = c(subphylum, class, order, family, genus, taxon),
               names_to = "group",
               values_to = "taxon") %>% 
  group_by(taxon, formula) %>% 
  summarize(a = mean(a),
            b = mean(b)) %>% 
  filter(formula == "M = aL^b") %>% 
  bind_rows(tibble(taxon = "daphnia",   # from Sterner et al https://aslopubs.onlinelibrary.wiley.com/doi/pdf/10.4319/lo.1993.38.4.0857
                   a = -2.7,
                   b = 2.57))

treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

temperature <- read_csv("data/tank_temperature.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy(date),
         month = month(date)) %>% 
  mutate(temp_deg_c = temperature_c) %>% 
  left_join(treatments) 

site_coords = c(42.800489,-96.926772)

# body size
# body_size_list <- import_list("data/body_size_data_summer_2023.xlsx", setclass = "tbl")
# 
# body_sizes = bind_rows(body_size_list[[1]], body_size_list[[2]]) %>% 
#   mutate(date = "2023-06-21") %>% 
  # mutate(taxon = case_when(grepl("chiro", taxon) ~ "chironomidae",
  #                          grepl("cole", taxon) ~ "coleoptera",
  #                          grepl("cerat", taxon) ~ "ceratopogonidae",
  #                          TRUE ~ taxon)) %>%
  # left_join(treatments) %>%
  # filter(!is.na(length_mm))

body_sizes = read_excel("data/experiment_dakota_02_04_2024_vg.xlsx") %>% 
  mutate(taxon = case_when(grepl("chiro", taxon) ~ "chironomidae",
                           grepl("cole", taxon) ~ "coleoptera",
                           grepl("cerat", taxon) ~ "ceratopogonidae",
                           TRUE ~ taxon)) %>%
  left_join(treatments) %>%
  filter(!is.na(length_mm))

write_csv(body_sizes, file = "data/body_sizes.csv")

# Plot temperature --------------------------------------------------------
treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

temperature <- read_csv("data/tank_temperature.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy(date),
         month = month(date)) %>% 
  mutate(temp_deg_c = temperature_c) %>% 
  left_join(treatments) 

temp_plot = temperature %>%  
  mutate(heat_label = case_when(heat == "heated" ~ "+heat",
                                  TRUE ~ "-heat"),
         heat_label = as.factor(heat_label),
         heat_label = fct_relevel(heat_label, "+heat")) %>% 
  ggplot(aes(x = date, y = temp_deg_c, color = heat_label)) + 
  geom_point(size = 0.3) + 
  geom_line(aes(group = tank), alpha = 0.5, linewidth = 0.2) +
  scale_color_brewer(type = "qual") + 
  theme_classic() + 
  labs(y = "Water temperature (\u00b0C)",
       color = "") +
  theme(legend.position = c(0.9, 0.5),
        text = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.title.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  NULL

ggview::ggview(temp_plot, width = 5, height = 4)
ggsave(temp_plot, file = "plots/temp_plot.jpg", width = 5, height = 4)

temperature %>% 
  group_by(date, heat) %>% 
  reframe(mean = mean(temp_deg_c)) %>% 
  pivot_wider(names_from = heat, values_from = mean) %>% 
  mutate(diff = heated - `no heated`) %>% 
  ggplot(aes(x = date, y = diff)) + 
  geom_point() +
  geom_line() +
  ylim(-10, 10) +
  geom_hline(yintercept = 0) +
  stat_label_peaks(aes(label = after_stat(y.label)),
                   span = 2,
                   position = position_nudge(y = 0.5),
                   color = "black") +
  stat_label_valleys(aes(label = after_stat(y.label)),
                   span = 2,
                   position = position_nudge(y = -0.5),
                   color = "black")

temperature %>% 
  group_by(date, heat) %>% 
  reframe(mean = mean(temp_deg_c)) %>% 
  pivot_wider(names_from = heat, values_from = mean) %>% 
  mutate(diff = heated - `no heated`) %>% 
  reframe(mean = mean(diff),
          sd = sd(diff))


temperature %>% 
  mutate(yday = yday(date)/min(yday(date))) %>% 
  mutate(week = week(date)/min(week(date))) %>% 
  filter(week == min(week) | week == max(week)) %>%
  group_by(week, heat) %>% 
  reframe(mean = mean(temp_deg_c))


metab = read_delim("data/gjoni_wesner_exp_2023.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)




