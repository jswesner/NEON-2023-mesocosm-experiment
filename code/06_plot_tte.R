library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)

# load posteriors
lambda = readRDS(file = "posteriors/lambdas.rds") %>%  
  mutate(heat= case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  mutate(lambda = .epred) %>% 
  ungroup %>% 
  select(heat, fish, .draw, lambda, .epred) %>% 
  mutate(metric = "lambda")

ppmr = readRDS("posteriors/ppmr_posts.rds") %>%  
  mutate(heat = case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  mutate(ppmr = .epred) %>% 
  ungroup %>% 
  select(heat, .draw, fish, ppmr, .epred) %>% 
  mutate(metric = "ppmr")

gamma = readRDS(file = "posteriors/scaling_posts.rds") %>% select(heat, fish, .draw, value) %>%  
  mutate(heat = case_when(heat == "heated" ~ "heat",
                                TRUE ~ "noheat")) %>% 
  mutate(gamma = value,
         .epred = gamma) %>% 
  mutate(metric = "gamma")

tte = lambda %>% select(-.epred, -metric) %>% 
  left_join(gamma %>% select(-.epred, -metric)) %>% 
  right_join(ppmr %>% select(-.epred, -metric)) %>% 
  mutate(subsidy = 0.4) %>% 
  mutate(te = (ppmr*1)^(lambda + 1 + gamma),
         te_theoretical_ppmr_scaling = 1e4^(lambda + 1 + 0.75)) %>% 
  select(-value)


tte %>% 
  ggplot(aes(x = te, fill = heat)) + 
  stat_halfeye() +
  scale_x_log10() + 
  geom_vline(xintercept = 0.1)

tte %>% 
  pivot_longer(cols = starts_with("te")) %>% 
  ggplot(aes(x = value, fill = heat, alpha = name)) + 
  stat_halfeye() +
  scale_x_log10() + 
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(xintercept = 0.1) 

posts_stacked = bind_rows(gammas, ppmr, gamma) %>% 
  select(-gamma, -ppmr, -value, -gamma)

gamma_posts = posts_stacked %>% 
  filter(fish == "fish") %>% 
  filter(metric == "gamma") %>% 
  ggplot(aes(x = .epred, fill = heat)) + 
  stat_halfeye() +
  labs(x = "gamma") +
  geom_vline(xintercept = 0.75)

ppmr_posts = posts_stacked %>% 
  filter(metric == "ppmr") %>% 
  ggplot(aes(x = .epred, fill = heat)) + 
  stat_halfeye() +
  labs(x = "ppmr") +
  geom_vline(xintercept = 1e5)

lambda_posts = posts_stacked %>% 
  filter(metric == "lambda") %>% 
  ggplot(aes(x = .epred, fill = heat)) + 
  stat_halfeye() +
  labs(x = "lambda") +
  geom_vline(xintercept = -1.95)

library(patchwork)

gamma_posts/ppmr_posts/lambda_posts



# Check ISD's with sizeSpectra --------


library(sizeSpectra)

dw <- read_csv("data/dw_fixed.csv") %>% 
  mutate(tank = as.integer(tank),
         tank_f = as.factor(tank))

dw_single = dw %>% filter(tank == 24)

eight.results = eightMethodsMEE(dw_single$dw_mg, num.bins = 12, b.only = F)

eight.results$hLBmiz.list$slope
eight.results$hMLE.list$b

MLE.plot(dw_single$dw_mg,
         b = eight.results$hMLE.list$b,
         # b = -1.13,
         log="xy")



brm_isd_rand$data %>% 
  distinct(xmin, xmax, tank_f, heat, fish) %>% 
  mutate(tank = parse_number(as.character(tank_f)),
         counts = 1) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NULL) %>% 
  group_by(tank) %>% 
  median_qi(.epred) %>% 
  print(n = Inf)

brm_isd_rand$data  %>% 
  mutate(tank = parse_number(as.character(tank_f))) %>% 
  group_by(tank) %>% 
  arrange(-dw_mg) %>% 
  mutate(order = row_number()) %>% 
  filter(tank == 12) %>% 
  ggplot(aes(x = dw_mg, y = order)) + 
  geom_point(shape = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_base()



x = dw_single$dw_mg
plot(sort(x, decreasing=TRUE),
       1:length(x),
       log = "xy",
       xlab = expression(paste("Values, ", italic(x))),
       ylab = expression( paste("Number of ", values >= x), sep=""))


         