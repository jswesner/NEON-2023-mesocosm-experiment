library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)
library(readxl)

# load model and data
brm_isd_rand = readRDS(file = "models/brm_isd_rand.rds")

dw <- read_csv("data/dw_fixed.csv") %>% 
  mutate(tank = as.integer(tank),
         tank_f = as.factor(tank))


# get posteriors
ind_trts = brm_isd_rand$data %>% 
  distinct(heat, fish, xmin, xmax) %>%
  mutate(counts = 1,
         heat_label = case_when(heat == "heated" ~ "+heat",
                                TRUE ~ "-heat")) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NA)

ind_rands = brm_isd_rand$data %>% 
  distinct(heat, fish, tank_f, xmin, xmax) %>%
  mutate(counts = 1,
         heat_label = case_when(heat == "heated" ~ "+heat",
                                TRUE ~ "-heat")) %>% 
  add_epred_draws(brm_isd_rand, re_formula = NULL)

ind_summary = ind_rands %>% 
  group_by(tank_f, heat, fish, heat_label) %>% 
  median_qi(.epred)

# plot lambdas

plot_2023 = ind_rands %>% 
  ggplot(aes(x = heat_label, y = .epred, fill = fish)) +
  geom_boxplot(data = ind_trts, outlier.shape = NA,
               width = 0.2, position = position_dodge(width = 0.3),
               alpha = 0.7) + 
  geom_pointinterval(data = ind_summary,
                     aes(y = .epred, color = fish, ymin = .lower, ymax = .upper),
                     position = position_jitterdodge(dodge.width = 0.3,
                                                     jitter.width = 0.05),
                     linewidth = 0.01, alpha = 0.2, 
                     size = 0.1) +
  theme_default() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(y = "\u03bb",
       x = "Temperature",
       fill = "",
       color = "") +
  theme(legend.position = "top",
        text = element_text(size = 9),
        legend.text = element_text(size = 7))

ggsave(plot_2023, file = "plots/plot_2023.jpg", width = 4, height = 4)                     

# calculate probabilities of difference

ind_trts %>%
  ungroup %>% 
  select(-xmin, -xmax, -counts, -heat_label, -.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = fish, values_from = .epred) %>% 
  mutate(diff = fish - `no fish`) %>% 
  group_by(heat) %>% 
  reframe(prob_diff = sum(diff<0)/max(.draw))

ind_trts %>% 
  group_by(fish, heat) %>% 
  median_qi(.epred)

ind_trts %>%
  ungroup %>% 
  select(-xmin, -xmax, -counts, -heat_label, -.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = heat, values_from = .epred) %>% 
  mutate(diff = heated - `no heated`) %>% 
  group_by(fish) %>%
  mutate(prob_diff = sum(diff>0)/max(row_number())) %>% 
  group_by(prob_diff, fish) %>% 
  median_qi(diff)

ind_trts %>%
  ungroup %>% 
  select(-xmin, -xmax, -counts, -heat_label, -.row, -.chain, -.iteration) %>%
  pivot_wider(names_from = fish, values_from = .epred) %>% 
  mutate(diff = `no fish` - fish) %>% 
  # group_by(heat) %>% 
  mutate(prob_diff = sum(diff>0)/max(row_number())) %>% 
  group_by(prob_diff, heat) %>% 
  median_qi(diff)

# check model and plot isds -------------------------------------------------------------

pp_check(brm_isd_rand, type = "boxplot") + scale_y_log10()


isd_preds = get_isd_posts(model = brm_isd_rand, group = c("tank_f", "heat", "fish"),
                          re_formula = NULL)

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
      expand(x = 10^seq(log10(min_x_group), log10(max_x_group), length.out = 300))
    
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
  geom_point(data = isd_dots, aes(size = x),
             shape = 1,
             alpha = 0.3) + 
  geom_line(aes(color = interaction(fish, heat),
                group = interaction(.draw, tank_f)),
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


line_preds %>% 
  left_join(tank_neworder) %>% 
  group_by(tank_f) %>% 
  mutate(x = scales::rescale(x)) %>% 
  ggplot(aes(x = x, y = prob_yx ))  +
  # geom_point(data = isd_dots, aes(size = x),
  # shape = 1,
  # alpha = 0.3) + 
  geom_line(aes(color = fish,
                group = interaction(.draw, tank_f)),
            alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ heat) +
  coord_cartesian(ylim = c(0.01, NA)) +
  guides(size = "none") +
  theme_default() +
  ggthemes::scale_color_colorblind(guide = guide_legend(override.aes = list(size = 3,
                                                                            alpha = 1) ) ) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  labs(x = "Individual Dry Mass (mg)")

isd_plots_2023

ggsave(isd_plots_2023, file = "plots/isd_plots_2023.jpg",
       width = 10, height = 7)



dw_2022 <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/NEON-mesocosm-experiment/data/dw_fixed.csv")

compare_2022_2023 = dw_2022 %>% mutate(year = "2022") %>%
  filter(correct_taxon != "Daphnia") %>% 
  bind_rows(dw %>% mutate(year = "2023")) %>% 
  ggplot(aes(x = tank, y = dw_mg)) + 
  geom_jitter(aes(color = year), size = 0.2) +
  facet_wrap(~year) +
  scale_y_log10()

compare_2022_2023

ggsave(compare_2022_2023, file = "plots/compare_2022_2023.jpg", width = 12, height = 6)

dw_2022 %>% mutate(year = "2022") %>% 
  filter(correct_taxon != "Daphnia") %>% 
  bind_rows(dw %>% mutate(year = "2023")) %>% 
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