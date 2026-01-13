library(brms)
library(tidyverse)
library(tidybayes) 
library(isdbayes)
theme_set(theme_default())

fit_isd = readRDS(file = "models/fit_isd.rds")
dat = fit_isd$data

post_dots = readRDS(file = "posteriors/post_dots.rds")

tank_lambdas = post_dots %>% 
  group_by(tank) %>% 
  median_qi(.epred)

write_csv(tank_lambdas, file = "tables/tank_lambdas.csv")


#iter = 4000, 
#chains = 4,
#threads = 12,
#cores = 32)


# summarize taxa ------
# ISD
dw <- read_csv("data/dw_fixed.csv") %>% 
  mutate(tank = as.integer(tank),
         tank_f = as.factor(tank),
         type = "ISD")

# metab
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
         log_resp_c = log_resp - mean(log_resp),
         type = "Metabolic Rate")

# ISD + metab
dw_metab = bind_rows(dw, metab %>% select(tank, taxon, treatment, dw_mg, type)) 

# percent by number
dw_metab %>% 
  group_by(type) %>% 
  add_tally(name = "total") %>% 
  group_by(taxon, total, type) %>% 
  tally() %>% 
  mutate(percent = n/total)

dw_metab %>% 
  # group_by(type) %>% 
  add_tally(name = "total") %>% 
  group_by(taxon, total) %>% 
  tally() %>% 
  mutate(percent = n/total)

dw_metab %>% 
  group_by(treatment) %>%
  add_tally(name = "total") %>% 
  group_by(taxon, total, treatment) %>% 
  tally() %>% 
  mutate(percent = n/total)

# percent by mass
dw_metab %>% 
  group_by(type) %>% 
  mutate(total = sum(dw_mg)) %>% 
  group_by(taxon, total, type) %>% 
  reframe(n = sum(dw_mg)) %>% 
  mutate(percent = n/total)

dw_metab %>% 
  # group_by(type) %>% 
  mutate(total = sum(dw_mg)) %>% 
  group_by(taxon, total) %>% 
  reframe(n = sum(dw_mg)) %>% 
  mutate(percent = n/total)

dw_metab %>% 
  group_by(heat) %>% 
  mutate(total = sum(dw_mg, na.rm = T)) %>% 
  group_by(taxon, total, heat) %>% 
  reframe(n = sum(dw_mg, na.rm = T)) %>% 
  mutate(percent = n/total)


# summary table of benthic mass
abund_summary = dw %>% 
  group_by(tank, fish, heat) %>% 
  add_tally(name = "total") %>%
  group_by(tank, fish, heat, taxon, total)  %>% 
  tally() %>% 
  mutate(percent = n/total*100) %>% 
  mutate(taxon = paste0(taxon, "_abundance")) %>% 
  select(tank, fish, heat, taxon, percent) %>% 
  pivot_wider(names_from = taxon, values_from = percent) %>% 
  arrange(fish, heat)

mass_summary = dw %>% 
  group_by(tank, fish, heat) %>% 
  mutate(total = sum(dw_mg)) %>% 
  group_by(tank, fish, heat, taxon, total)  %>% 
  reframe(n = sum(dw_mg)) %>% 
  mutate(percent = n/total) %>% 
  mutate(taxon = paste0(taxon, "_biomass")) %>% 
  select(tank, fish, heat, taxon, percent) %>% 
  pivot_wider(names_from = taxon, values_from = percent) %>% 
  arrange(fish, heat)

abundance_mass = left_join(abund_summary, mass_summary)

community_table = abundance_mass %>% 
  select(tank, fish, heat, starts_with("chir"), starts_with("odon"))

write_csv(community_table, file = "tables/community_table.csv")
