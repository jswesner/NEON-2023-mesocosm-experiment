library(poweRlaw)
library(tidyverse)
# this method follows Clauset et al. 2009 by estimating the minimum
# size for which the data follow a power law using by minimizing the K-S statistic.
# i.e., xmin is model-based

# this example is for a single sample of data

# 1) load data
d = read_csv(file = "data/dw_raw.csv") %>% filter(tank == 1)

# 2) resample (only needed if counts is variable)
# dat = dat_all %>% 
#   group_by(tank) %>% 
#   sample_n(5000, weight = counts, replace = T)

# 3) create powerlaw object with poweRlaw package
powerlaw = conpl$new(d$dw_mg)

# 4) use Komogorov-Smirnov to find xmin value that maximizes fit to power law

xmin_clauset = estimate_xmin(powerlaw)$xmin

# 5) add new xmins and filter 
# Now use this data to estimate lambda with isdbayes. 
d_new = d %>% mutate(xmin_clauset = xmin_clauset) %>% 
  filter(dw_mg >= xmin_clauset) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg))

