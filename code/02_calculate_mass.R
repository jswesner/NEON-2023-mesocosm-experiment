library(tidyverse)

# load data ---------------------------------------------------------------
lengths_fixed = read_csv(file = "data/body_sizes.csv") %>% 
  mutate(taxon)

macro_lw_coeffs = read_csv("data/macro_lw_coeffs.csv") %>% 
  mutate(taxon = str_to_lower(correct_taxon))# published length-weight coefficients

# convert mm length to mg mass
dw_raw = lengths_fixed %>% 
  left_join(macro_lw_coeffs) %>% 
  mutate(dw_mg = a*length_mm^b) %>% 
  ungroup %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg),
         counts = 1)
  
write_csv(dw_raw, file = "data/dw_raw.csv")

