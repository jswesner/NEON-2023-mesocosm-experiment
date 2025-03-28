library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)
library(janitor)
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
body_size_list <- import_list("data/body_size_data_summer_2023.xlsx", setclass = "tbl")

body_sizes = bind_rows(body_size_list[[1]], body_size_list[[2]]) %>% 
  mutate(date = "2023-06-21") %>% 
  mutate(taxon = case_when(grepl("chiro", taxon) ~ "chironomidae",
                           grepl("cole", taxon) ~ "coleoptera",
                           grepl("cerat", taxon) ~ "ceratopogonidae",
                           TRUE ~ taxon)) %>% 
  left_join(treatments) %>% 
  filter(!is.na(length_mm))

write_csv(body_sizes, file = "data/body_sizes.csv")

# Plot temperature --------------------------------------------------------
temp_plot = temperature %>% 
  filter(date >= "2022-05-27") %>% 
  ggplot(aes(x = date, y = temp_deg_c, color = treatment)) + 
  geom_point(size = 0.3) + 
  geom_line(aes(group = tank), alpha = 0.5, linewidth = 0.2) +
  scale_color_colorblind() + 
  theme_classic() + 
  # scale_y_log10() +
  NULL

ggsave(temp_plot, file = "plots/temp_plot.jpg", width = 5, height = 3)



# Plot O2_temp ------------------------------------------------------------
library(ggthemes)
library(viridis)
temperature %>%
  clean_names() %>% 
  filter(!is.na(o2_do_mg_l)) %>% 
  ggplot(aes(y = o2_do_mg_l, x = date_time)) +
  geom_point(aes(color = treatment)) + 
  geom_line(aes(group = tank, color = treatment)) +
  facet_wrap(~treatment)
  



metab = read_delim("data/gjoni_wesner_exp_2023.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)




