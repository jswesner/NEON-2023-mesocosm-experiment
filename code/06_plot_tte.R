library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)

nsims = 10

library(viridis)

sim_te = tibble(lambda = seq(-3, -1, length.out = 30)) %>% 
  expand_grid(log10ppmr = seq(log10(1e2), log10(1e6), length.out = nsims)) %>% 
  # expand_grid(te = seq(0.01, 1, length.out = nsims)) %>% 
  expand_grid(gamma = c(0.3, 0.75, 1)) %>% 
  # mutate(log10ppmr = log10(ppmr)) %>% 
  mutate(log10te = (lambda + gamma + 1)*log10ppmr,
         # log10te_subsidy = 10^((lambda + gamma + 1 - subsidy)*log10ppmr),
         te = 10^log10te,
         ppmr = 10^log10ppmr) %>% 
  mutate(theoretical_te = case_when(te >= 0.05 & te <= 0.4 ~ "in", TRUE ~ "out")) %>% 
  mutate(gamma_labeled = paste0("\u03b3 = ", gamma))

label_lambdas = tibble(gamma_labeled = paste0("\u03b3 = ", 0.3),
                       te = min(sim_te$te),
                       lambda = -2.1,
                       label = "Empirical \u03bb's -->")

label_tte = tibble(gamma_labeled = paste0("\u03b3 = ", 0.3),
                   te = 0.115,
                   lambda = -2.5,
                   label = "Plausible TTE's")

post_medians = read_rds("posteriors/post_dots.rds") %>% 
  group_by(tank) %>% 
  median_qi(.epred)

simulated_tte_plot = sim_te %>% 
  ggplot(aes(x = lambda, y = te)) + 
  geom_point(aes(alpha = theoretical_te, color = log10ppmr), size = 0.4) +
  # geom_line(aes(group = ppmr), alpha = 0.4) +
  scale_y_log10(breaks = c(0.0001,0.01, 0.05, 0.4, 1, 100, 10000, 1e6, 1e10),
                labels = c("0.0001","0.01", "0.05", "0.4", "1", "100", "10,000", "1,000,000", "10,000,000,000")) +
  facet_wrap(~gamma_labeled) +
  scale_alpha_manual(values = c(1, 0.1)) +
  geom_hline(yintercept = c(0.05, 0.4), linetype = "dashed", linewidth = 0.3) +
  brms::theme_default() +
  scale_color_viridis(labels = c("10e2", "10e3", "10e4", "10e5", "10e6")) +
  # geom_vline(xintercept = rnorm(500, -1.2, 0.1),
             # alpha = 0.007) +
  geom_vline(data = post_medians, aes(xintercept = .epred), alpha = 0.1) +
  labs(y = "Trophic Transfer Efficiency",
       x = "\u03bb (ISD exponent)",
       color = "PPMR",
       caption = "Figure X. Simulation study of trophic transfer efficiency under different scenarios of PPMR, metabolic scaling (\u03b3), and ISD exponenents (\u03bb).
       Vertical gray lines indicate the empirical lambda measured in each mesocosm tank.") +
  guides(alpha = "none") +
  geom_text(data = label_lambdas, aes(label = label), size = 2) +
  geom_text(data = label_tte, aes(label = label), size = 2) +
  theme(legend.text = element_text(size = 7),
        text = element_text(size = 9),
        legend.position = c(0.05, 0.85),
        # legend.key.width=unit(3,"cm"),
        legend.key.size = unit(0.2, "cm"))

# ggview::ggview(simulated_tte_plot, width = 6.5, height = 3.5)
ggsave(simulated_tte_plot, width = 6.5, height = 3.5,
       file = "plots/simulated_tte_plot.jpg", dpi = 400)
