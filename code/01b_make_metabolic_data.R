library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
library(ggthemes)
theme_set(theme_default())

# get data
treatments = read_csv("data/treatments.csv") %>% 
  mutate(treatment = paste0(heat, "_",fish))

macro_lw_coeffs <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra/data/raw_data/inverts/macro_lw_coeffs.csv") %>% 
  pivot_longer(cols = c(subphylum, class, order, family, genus, taxon),
               names_to = "group",
               values_to = "taxon") %>% 
  glimpse() %>% 
  group_by(taxon, formula) %>% 
  reframe(a = mean(a),
          b = mean(b)) %>% 
  filter(formula == "M = aL^b") %>% 
  bind_rows(tibble(taxon = "daphnia",   # from Sterner et al https://aslopubs.onlinelibrary.wiley.com/doi/pdf/10.4319/lo.1993.38.4.0857
                   a = -2.7,
                   b = 2.57)) %>% 
  ungroup %>% 
  mutate(taxon = tolower(taxon))

metab = read_delim("data/gjoni_wesner_exp_2023.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  separate(species, into = c("taxon", "stage")) %>% 
  left_join(treatments) %>% 
  left_join(macro_lw_coeffs) %>% 
  ungroup %>% 
  mutate(ww_mg = a*length^b,
         dw_mg = ww_mg*0.2,
         log_dw = log(dw_mg),
         log_resp = log(resp_rate),
         log_dw_c = log_dw - mean(log_dw),
         log_resp_c = log_resp - mean(log_resp))

write_csv(metab, file = "data/metabolism.csv")


metab %>% 
  ggplot(aes(x = log_dw_c, y = log_resp_c)) +
  geom_point(aes(color = treatment)) +
  geom_smooth(method = "lm", aes(fill = treatment)) 