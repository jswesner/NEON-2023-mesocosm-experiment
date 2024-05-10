library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
library(ggthemes)
theme_set(theme_default())

# 1) get data
metab = read_csv(file = "data/metabolism.csv") %>% 
  ungroup %>% 
  mutate(fish_range = case_when(log_dw <= 0.26 & log_dw >= 0.5 ~ "fish_food",
                                TRUE ~ "not fish food")) %>% 
  group_by(treatment) %>% 
  mutate(dw_mg_c = scale(dw_mg, scale = F),
         max_dw = max(dw_mg),
         dw_mg_1 = dw_mg/max(dw_mg),
         max_dw_1 = max(dw_mg_1),
         min_dw_1 = min(dw_mg_1),
         max_resp = max(resp_rate),
         resp_1 = resp_rate/max(resp_rate)) %>% 
  filter(!is.na(taxon)) %>% 
  mutate(trt_order = case_when(treatment == "heated_fish" ~ "fish\nheated",
                               treatment == "heated_no fish" ~ "no fish\nheat",
                               treatment == "no heated_fish" ~ "fish\nno heat",
                               TRUE ~ "no fish\nnoheat"))

heat_fish = metab %>% ungroup %>% distinct(fish, heat, treatment)

# simulate priors
nrep = 100
prior_power = tibble(a = rlnorm(nrep, log(1), 0.5),
                     b = rlnorm(nrep, log(0.75), 0.2),
                     iter = 1:nrep)  %>% 
  expand_grid(treatment = c("a", "b", "c", "d")) %>% 
  mutate(b_trt = rnorm(nrow(.), 0, 0.11),
         a_trt = rnorm(nrow(.), 0, 0.11)) %>% 
  expand_grid(x = seq(min(metab$dw_mg_1), max(metab$dw_mg_1), length.out = 100)) %>% 
  mutate(b_b_trt = b + b_trt,
         a_a = a + a_trt) %>% 
  mutate(y = a_a*x^b_b_trt, 
         ylog = log(y))


prior_power %>% 
  ggplot(aes(x = x, y = exp(ylog), color = treatment)) + 
  geom_line(aes(group = iter)) +
  facet_wrap(~treatment) +
  # scale_x_log10() + 
  # scale_y_log10() +
  NULL


# fit model (identity link worked better than log or inverse in trials)
prior1 <- c(set_prior('lognormal(0, 0.5)', nlpar = "a", coef = "Intercept"),
            set_prior('lognormal(-0.29, 0.5)', nlpar = "b", coef = "Intercept"),
            set_prior('normal(0, 0.1)', nlpar = "b"),
            set_prior('normal(0, 0.1)', nlpar = "a"))

fit_test_gamma <-
  brm(
    bf(
      resp_1 ~ a * dw_mg_1 ^ b ,
      a ~ treatment,
      b ~ treatment,
      nl = TRUE
    ),
    data = metab,
    iter = 1000,
    cores = 1,
    family = Gamma(link = "identity"),
    prior = prior1
  )

fit_test_gamma = readRDS(file = "models/fit_test_gamma.rds")

saveRDS(fit_test_gamma, file = "models/fit_test_gamma.rds")

fit_test_gamma = readRDS(file = "models/fit_test_gamma.rds")

plot(conditional_effects(fit_test_gamma), points= T)

pp_check(fit_test_gamma, type = "boxplot")

# posteriors
pred_data = tibble(dw_mg_1 = seq(min(metab$dw_mg_1),
                max(metab$dw_mg_1), length.out = 20)) %>%
  expand_grid(treatment = unique(metab$treatment)) %>%
  left_join(metab %>% ungroup %>% distinct(treatment, max_dw_1, min_dw_1, max_dw,
                                           max_resp)) %>%
  filter(dw_mg_1 <= max_dw_1) %>%
  filter(dw_mg_1 >= min_dw_1) %>% 
  mutate(trt_order = case_when(treatment == "heated_fish" ~ "fish\nheated",
                               treatment == "heated_no fish" ~ "no fish\nheat",
                               treatment == "no heated_fish" ~ "fish\nno heat",
                               TRUE ~ "no fish\nnoheat")) %>% 
  left_join(heat_fish)

gamma_epred = pred_data %>%
  add_epred_draws(fit_test_gamma)

gamma_predict = pred_data %>%
  add_predicted_draws(fit_test_gamma)


gamma_epred %>%
  filter(.draw <= 100) %>%
  ggplot(aes(x = dw_mg_1*max_dw, y = .epred*max_resp)) +
  stat_lineribbon(.width = 0.95, alpha = 0.2) +
  facet_wrap(~treatment) +
  geom_point(data = metab, aes(y = resp_rate, size = dw_mg, color = taxon),
             shape = 1) +
  geom_vline(xintercept = c(0.018, 0.3)) +
  scale_x_log10() +
  scale_y_log10() +
  NULL

nonlinear_jacob_plot = gamma_epred %>%
  filter(.draw <= 100) %>%
  ggplot(aes(x = dw_mg_1*max_dw, y = .epred*max_resp))+
  geom_point(data = metab, aes(y = resp_rate, size = dw_mg, color = heat),
             shape = 1)  +
  stat_lineribbon(.width = 0.95, alpha = 0.4, aes(fill = heat)) +
  facet_wrap(~trt_order, ncol = 4) +
  labs(x = "Log Dry Mass (g)",
       y = "Log Metabolic Rate")

ggsave(nonlinear_jacob_plot, file = "plots/nonlinear_jacob_plot.jpg", width = 6.5, height = 2.5)




gamma_predict %>% 
  filter(.draw <= 100) %>% 
  ggplot(aes(x = dw_mg_1*max_dw, y = .prediction*max_resp)) + 
  stat_lineribbon(.width = 0.95, alpha = 0.2) +
  facet_wrap(~treatment) +
  geom_point(data = metab, aes(y = resp_rate, size = dw_mg, color = taxon),
             shape = 1) 

# plot exponents
slope_posts = as_draws_df(fit_test_gamma) %>% 
  mutate(slope_heated_fish = b_b_Intercept,
         slope_heated_nofish = b_b_Intercept + b_b_treatmentheated_nofish,
         slope_noheat_nofish = b_b_Intercept + b_b_treatmentnoheated_nofish,
         slope_noheat_fish = b_b_Intercept + b_b_treatmentnoheated_fish) %>% 
  pivot_longer(starts_with("slope")) %>% 
  mutate(name = str_sub(name, 7, 30)) %>% 
  separate(name, into = c("heat", "fish")) %>% 
  mutate(heat = as.factor(heat),
         heat = fct_relevel(heat, "noheat", "heated"))

exponent_posts = slope_posts %>% 
  ggplot(aes(x = heat, y = value, color = fish)) +
  stat_pointinterval() + 
  geom_line(data = . %>% filter(.draw <= 100),
            aes(group = interaction(fish, .draw)),
            alpha = 0.1) +
  labs(y = "Metabolic Scaling Exponent")


ggsave(exponent_posts, file = "plots/exponent_posts.jpg")


slope_posts %>% select(.draw, heat, fish, value) %>% 
  pivot_wider(names_from = fish, values_from = value) %>% 
  mutate(diff = fish - nofish) %>% 
  group_by(heat) %>% 
  mutate(max_draw = max(.draw)) %>% 
  reframe(probs = sum(diff < 0)/max_draw) %>% 
  distinct()

#test with varying intercepts
prior2 <- c(set_prior('lognormal(0, 0.5)', nlpar = "a", coef = "Intercept"),
            set_prior('lognormal(-0.29, 0.5)', nlpar = "b", coef = "Intercept"),
            set_prior('normal(0, 0.1)', nlpar = "b"),
            set_prior('normal(0, 0.1)', nlpar = "a"),
            set_prior('exponential(5)', class = "sd", nlpar = "a"),
            set_prior('exponential(5)', class = "sd", nlpar = "b"))


# fit_test_gammarand <-
#   brm(
#     bf(
#       resp_1 ~ a * dw_mg_1 ^ b ,
#       a ~ treatment + (1|taxon),
#       b ~ treatment + (1|taxon),
#       nl = TRUE
#     ),
#     data = metab,
#     iter = 1000,
#     cores = 1,
#     family = Gamma(link = "identity"),
#     prior = prior2
#   )

fit_test_gammarand = update(fit_test_gammarand, newdata = metab)

saveRDS(fit_test_gammarand, file = "models/fit_test_gammarand.rds")

plot(conditional_effects(fit_test_gammarand), points = T)

# posteriors
pred_datarand = tibble(dw_mg_1 = seq(min(metab$dw_mg_1),
                                 max(metab$dw_mg_1), length.out = 20)) %>%
  expand_grid(treatment = unique(metab$treatment)) %>%
  expand_grid(taxon = unique(metab$taxon)) %>% 
  left_join(metab %>% ungroup %>% distinct(treatment, max_dw_1, min_dw_1, max_dw,
                                           max_resp)) %>%
  filter(dw_mg_1 <= max_dw_1) %>%
  filter(dw_mg_1 >= min_dw_1)

gamma_epredrand = pred_datarand %>%
  add_epred_draws(fit_test_gammarand)

gamma_epredrand_overall = pred_data %>%
  add_epred_draws(fit_test_gammarand, re_formula = NA) 

gamma_predictrand = pred_datarand %>%
  add_predicted_draws(fit_test_gammarand)


gamma_epredrand %>%
  filter(.draw <= 100) %>%
  ggplot(aes(x = dw_mg_1*max_dw, y = .epred*max_resp)) +
  stat_lineribbon(.width = 0.95, alpha = 0.2, aes(fill = taxon)) +
  facet_wrap(~treatment) +
  geom_point(data = metab, aes(y = resp_rate, size = dw_mg, color = taxon),
             shape = 1) +
  # stat_lineribbon(.width = 0.95, data = gamma_epredrand_overall, fill = "gray",
                  # alpha = 0.2) +
  NULL


non_linear_preds = gamma_epredrand_overall %>%
  filter(.draw <= 100) %>%
  ggplot(aes(x = dw_mg_1*max_dw, y = .epred*max_resp)) +
  stat_lineribbon(.width = 0.95, alpha = 0.3) +
  facet_wrap(~treatment) +
  geom_point(data = metab, aes(y = resp_rate, size = dw_mg, color = taxon),
             shape = 1, alpha = 0.8) +
  labs(x = "mgDM", y = "Respiration Rate") +
  NULL


ggsave(non_linear_preds, file = 'plots/non_linear_preds.jpg')

