library(tidyverse)
# the next package (brms) won't work unless you've first installed RStan (https://mc-stan.org/users/interfaces/rstan.html)
# 1) install RStan (read the instructions carefully)
# 2) install brms via install.packages("brms")
library(brms) 
library(isdbayes)
library(tidybayes)

# model isd ---------------------------------------------------------------
library(readxl)

dw <- read_csv("data/dw_fixed.csv") %>% 
  mutate(tank = as.integer(tank),
         tank_f = as.factor(tank))

# 
# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ heat*fish, 
#               data = dw,
#               stanvars = stanvars,  # new thing added by the package
#               family = paretocounts(), # new thing added by the package
#               chains = 1, iter = 1000,
#               file = "models/brm_isd.rds",
#               prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
#                         prior(normal(0, 0.1), class = "b")))


brm_isd = readRDS("models/brm_isd.rds")

brm_isd = update(brm_isd, newdata = dw)

brm_isd_rand = update(brm_isd, newdata = dw, 
                 formula = . ~ heat*fish + (1|tank_f),
                 prior = c(prior(normal(-1.25, 0.2), class = "Intercept"),
                           prior(normal(0, 0.1), class = "b"),
                           prior(exponential(6), class = "sd")),
                 chains = 1, iter = 1000)

saveRDS(brm_isd, file = "models/brm_isd.rds")
saveRDS(brm_isd_rand, file = "models/brm_isd_rand.rds")

conditional_effects(brm_isd)

pp_check(brm_isd, type = "boxplot") + scale_y_log10()


ind_trts = brm_isd_rand$data %>% 
  distinct(heat, fish, xmin, xmax) %>%
  mutate(counts = 1) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NA)

ind_rands = brm_isd_rand$data %>% 
  distinct(heat, fish, tank_f, xmin, xmax) %>%
  mutate(counts = 1) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NULL)

ind_summary = ind_rands %>% 
  group_by(tank_f, heat, fish) %>% 
  median_qi(.epred)

plot_2023 = ind_rands %>% 
  ggplot(aes(x = heat, y = .epred, fill = fish)) +
  geom_boxplot(data = ind_trts, outlier.shape = NA,
               width = 0.2, position = position_dodge(width = 0.5),
               alpha = 0.7) + 
  geom_pointinterval(data = ind_summary,
                     aes(y = .epred, color = fish, ymin = .lower, ymax = .upper),
                     position = position_jitterdodge(dodge.width = 0.5,
                                                     jitter.width = 0.05),
                     linewidth = 0.2, alpha = 0.2) +
  theme_default() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(y = "\u03bb",
       x = "Temperature",
       fill = "",
       color = "")
                     
ggsave(plot_2023, file = "plots/plot_2023.jpg", width = 6, height = 5)                     


# check model -------------------------------------------------------------

pp_check(brm_isd_rand, type = "boxplot") + scale_y_log10()


isd_preds = get_isd_posts(model = brm_isd_rand, group = c("tank_f", "heat", "fish"),
                          re_formula = NULL)


isd_preds %>% 
  ggplot(aes(x = x, y = prob_yx)) + 
  geom_line(aes(group = tank_f))



posts = brm_isd_rand$data %>%
  group_by(tank_f) %>%
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>%
  distinct(fish, heat, tank_f, xmin, xmax) %>%
  mutate(counts = 1) %>%
  add_epred_draws(brm_isd_rand, re_formula = NULL, ndraws = 50)

line_preds = brm_isd_rand$data %>%
  group_by(across(c("fish", "heat", "tank_f")), xmin, xmax) %>%
  add_tally() %>%
  distinct(across(c("fish", "heat", "tank_f")), n) %>%
  group_by(across(c("fish", "heat", "tank_f")), n) %>%
  mutate(min_x_group = min(xmin), max_x_group = max(xmax)) %>%
  group_modify(function(df, .keep = "unused") {
    # Create a new column "x" with a logarithmic sequence between min_x_group and max_x_group
    df <- df %>%
      expand(x = 10^seq(log10(min_x_group), log10(max_x_group), length.out = 50))
    
    return(df)
  }) %>%
  left_join(posts,
            relationship = "many-to-many") %>%
  mutate(prob_yx = (1 - (x^(.epred + 1) - (xmin^(.epred+1)))/((xmax)^(.epred + 1) - (xmin^(.epred+1)))),
         n_yx = prob_yx*n) %>% 
  filter(x <= xmax) %>% 
  filter(x >= xmin)


tank_neworder = brm_isd_rand$data %>% distinct(tank_f, fish, heat) %>% 
  group_by(fish, heat) %>% 
  arrange(fish, heat) %>% 
  mutate(trt_id = row_number())


isd_dots = dw %>% 
  arrange(tank_f, -dw_mg) %>% 
  group_by(tank_f) %>% 
  mutate(n_yx = row_number(),
         x = dw_mg) %>% 
  left_join(tank_neworder)
  

isd_plots_2023 = line_preds %>% 
  left_join(tank_neworder) %>% 
  ggplot(aes(x = x, y = n_yx ))  +
  geom_point(data = isd_dots, aes(color = correct_taxon,
                                  size = x),
             shape = 16,
             alpha = 0.5) + 
  geom_line(aes(group = interaction(.draw, tank_f)),
            alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  facet_grid(interaction(fish, heat) ~ trt_id) +
  coord_cartesian(ylim = c(1, NA)) +
  guides(size = "none") +
  theme_default() +
  ggthemes::scale_color_colorblind(guide = guide_legend(override.aes = list(size = 3,
                                                                            alpha = 1) ) ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  labs(x = "Individual Dry Mass (mg)")

isd_plots_2023

ggsave(isd_plots_2023, file = "plots/isd_plots_2023.jpg",
       width = 10, height = 7)



dw_2022 <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/NEON-mesocosm-experiment/data/dw_fixed.csv")
compare_2022_2023 = dw_2022 %>% mutate(year = "2022") %>%
  filter(correct_taxon != "Daphnia") %>% 
  bind_rows(dw_fixed %>% mutate(year = "2023")) %>% 
  ggplot(aes(x = tank, y = dw_mg)) + 
  geom_jitter(aes(color = year), size = 0.2) +
  facet_wrap(~year) +
  scale_y_log10()

compare_2022_2023

ggsave(compare_2022_2023, file = "plots/compare_2022_2023.jpg", width = 12, height = 6)

dw_2022 %>% mutate(year = "2022") %>% 
  filter(correct_taxon != "Daphnia") %>% 
  bind_rows(dw_fixed %>% mutate(year = "2023")) %>% 
  ggplot(aes(x = dw_mg)) + 
  geom_histogram(aes(color = year)) +
  scale_x_log10() +
  facet_wrap(tank~year)


dw_2022 %>% 
  group_by(tank) %>% 
  arrange(-dw_mg) %>% 
  mutate(order = row_number()) %>% 
  ggplot(aes(x = dw_mg, y = order)) + 
  geom_point(aes(color = correct_taxon, size = dw_mg)) +
  scale_x_log10() + 
  scale_y_log10() +
  facet_wrap(~treatment)
  

