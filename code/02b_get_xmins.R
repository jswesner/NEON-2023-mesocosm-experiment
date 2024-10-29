library(poweRlaw)
library(tidyverse)
# this method follows Clauset et al. 2009 by estimating the minimum
# size for which the data follow a power law using by minimizing the K-S statistic.
# i.e., xmin is model-based

# 1) load data
dat_all = read_csv(file = "data/dw_raw.csv")

# 2) resample (only needed if no_m2 is variable)
# dat = dat_all %>% 
#   group_by(tank) %>% 
#   sample_n(5000, weight = counts, replace = T)

# 3) make a list of samples
dat_list = dat_all %>% group_by(tank) %>% group_split()

# 4) loop through samples and estimate xmin for each one
xmin_list = list()

for(i in 1:length(dat_list)){
  powerlaw = conpl$new(dat_list[[i]]$dw_mg)
  xmin_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                          tank = unique(dat_list[[i]]$tank))
}

# 5) combine the samples to a tibble
xmins_clauset = bind_rows(xmin_list)

# 6) add new xmins and filter 
dat_clauset_xmins = dat_all %>% left_join(xmins_clauset) %>% 
  group_by(tank) %>% 
  filter(dw_mg >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw_mg))

# 7) save
saveRDS(dat_clauset_xmins, file = "data/dat_clauset_xmins.rds")
