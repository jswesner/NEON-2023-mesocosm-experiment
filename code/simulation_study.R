library(tidyverse)
library(brms)
library(tidybayes)
library(ggridges)
library(ggthemes)
theme_set(theme_default())

# simulate data
n_sizes = 300
# x = rgamma(n = n_sizes, shape = 2, rate = 10) # simulates between ~ 0 and 1
x = rlnorm(n = n_sizes, meanlog = log(2/10), sd = 0.5) # simulates between ~ 0 and 1
a = c(1)
b = c(0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)

sim_metab = tibble(x = x,
                   ind = 1:n_sizes) %>% 
  expand_grid(a = a) %>% 
  expand_grid(b = b) %>% 
  mutate(params = paste0("a = ", a, ", b = ", b)) %>% 
  mutate(mean_y = a*x^b, 
         rate = 30,
         shape = mean_y*rate,
         # y = rgamma(nrow(.), shape = shape, rate = rate),
         y = rlnorm(nrow(.), log(mean_y), sd = 0.05),
         logx = log(x),
         logy = log(y)) %>% 
  group_by(params) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup

# plot simulated data
sim_metab %>% 
  ggplot(aes(x = x, y = mean_y)) +
  geom_line(aes(group = params, color = params)) +
  geom_point(aes(y = y), size = 0.1, shape = 1) +
  facet_wrap(~b)

sim_metab %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(group = params, color = params)) +
  geom_point(aes(y = y), size = 0.1, shape = 0.1) +
  facet_wrap(~b) +
  scale_x_log10() +
  scale_y_log10()


# fit models
prior_sim <- c(set_prior('lognormal(0, 0.5)', nlpar = "a", coef = "Intercept"),
            set_prior('lognormal(-0.29, 0.5)', nlpar = "b", coef = "Intercept"))


sim_metab_list = sim_metab %>% group_by(params) %>% group_split

# fit_sim_gamma <-
#   brm(
#     bf(
#       y ~ a * x ^ b ,
#       a + b ~ 1,
#       nl = TRUE
#     ),
#     data = sim_metab_list[[1]],
#     iter = 1000,
#     cores = 1,
#     family = Gamma(link = "identity"),
#     prior = prior_sim
#   )
# fit_lm_gaussian = brm(logy ~ logx, data = sim_metab_list[[1]])
# saveRDS(fit_sim_gamma, file = "models/fit_sim_gamma.rds")
# saveRDS(fit_lm_gaussian, file = "models/fit_lm_gaussian.rds")

fit_sim_gamma = readRDS(file = "models/fit_sim_gamma.rds")
fit_lm_gaussian = readRDS(file = "models/fit_lm_gaussian.rds")


# loop through different b's
# sim_metab = tibble(x = x,
#                    ind = 1:n_sizes) %>% 
#   expand_grid(a = a) %>% 
#   expand_grid(b = b) %>% 
#   mutate(params = paste0("a = ", a, ", b = ", b)) %>% 
#   mutate(mean_y = a*x^b, 
#          rate = 30,
#          shape = mean_y*rate,
#          y = rgamma(nrow(.), shape = shape, rate = rate),
#          logx = log(x),
#          logy = log(y)) %>% 
#   group_by(params) %>% 
#   mutate(id = cur_group_id()) %>% 
#   ungroup

sim_metab = tibble(x = x,
                   ind = 1:n_sizes) %>% 
  expand_grid(a = a) %>% 
  expand_grid(b = b) %>% 
  mutate(params = paste0("a = ", a, ", b = ", b)) %>% 
  mutate(mean_y = a*x^b, 
         rate = 30,
         shape = mean_y*rate,
         # y = rgamma(nrow(.), shape = shape, rate = rate),
         y = rlnorm(nrow(.), log(mean_y), sd = 0.05),
         logx = log(x),
         logy = log(y)) %>% 
  group_by(params) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup


sim_metab_list = sim_metab %>% group_by(params) %>% group_split

fit_sims = NULL
lm_sims = NULL

for(i in 1:length(sim_metab_list)){
  fit_sims[[i]] = update(fit_sim_gamma, newdata = sim_metab_list[[i]],
                         data2 = list(b = unique(sim_metab_list[[i]]$b),
                                      likelihood = "gamma"))
  
  lm_sims[[i]] = update(fit_lm_gaussian, newdata = sim_metab_list[[i]],
                        data2 = list(b = unique(sim_metab_list[[i]]$b),
                                     likelihood = "gaussian"))
}
saveRDS(fit_sims, file = "models/fit_sims.rds")

# get posteriors

post_list_gamma = NULL
post_list_lm = NULL

for(i in 1:length(fit_sims)){
  post_list_gamma[[i]] =  as_draws_df(fit_sims[[i]]) %>%
    mutate(true_b = fit_sims[[i]]$data2$b,
           likelihood = fit_sims[[i]]$data2$likelihood)
  
  post_list_lm[[i]] =  as_draws_df(lm_sims[[i]]) %>%
    mutate(true_b = lm_sims[[i]]$data2$b,
           likelihood = lm_sims[[i]]$data2$likelihood)
}

post_preds_gamma = bind_rows(post_list_gamma) %>% 
  mutate(b = b_b_Intercept)
post_preds_lm = bind_rows(post_list_lm) %>% 
  mutate(b = b_logx)

post_preds = bind_rows(post_preds_gamma, post_preds_lm)

post_preds %>% 
  ggplot(aes(x = true_b, y = b, fill = likelihood)) + 
  stat_halfeye(aes(group = interaction(true_b, likelihood))) +
  geom_abline()

mod = 1
pp_check(fit_sims[[mod]])
pp_check(lm_sims[[mod]])
pp_check(fit_sims[[mod]], type = "stat")
pp_check(lm_sims[[mod]], type = "stat")


gamma_cond = plot(conditional_effects(fit_sims[[mod]]), points = T)
lm_cond = plot(conditional_effects(lm_sims[[mod]]), points = T)

gamma_cond

gamma_epred = tibble(x = seq(min(fit_sims[[mod]]$data$x),
                             max(fit_sims[[mod]]$data$x),
                             length.out = 30)) %>% 
  add_epred_draws(fit_sims[[mod]]) 


lm_epred = tibble(logx = seq(min(lm_sims[[mod]]$data$logx),
                             max(lm_sims[[mod]]$data$logx),
                             length.out = 30)) %>% 
  add_epred_draws(lm_sims[[mod]]) %>% 
  mutate(exp_epred = exp(.epred))

gamma_epred %>% 
  ggplot(aes(x = x, y = .epred)) + 
  stat_lineribbon(.width = 0.95, alpha = 0.2) + 
  geom_point(data = fit_sims[[mod]]$data, aes(y = y))

lm_epred %>% 
  ggplot(aes(x = exp(logx), y = exp_epred)) + 
  stat_lineribbon(.width = 0.95, alpha = 0.2) + 
  geom_point(data = lm_sims[[mod]]$data, aes(y = exp(logy)))

lm_epred %>% 
  ggplot(aes(x = logx, y = .epred)) + 
  stat_lineribbon(.width = 0.95, alpha = 0.2) + 
  geom_point(data = lm_sims[[mod]]$data, aes(y = logy))



# compare groups ----------------------------------------------------------

sim_group = sim_metab %>% filter(b == 0.35 | b == 0.65) %>% 
  mutate(b_fac = as.factor(b),
         y = 100*y,
         logy = log(y))

sim_group %>% 
  ggplot(aes(x = x, y = y, color = b_fac)) + 
  geom_point() 

# 
# brm_lognorm = brm(bf(y ~ a*x^b,
#                      a ~ b_fac,
#                      b ~ b_fac,
#                      nl = TRUE),
#                   data = sim_group,
#                   family = lognormal(link = "identity"),
#                   # prior = c(prior(normal(1, 0.2), nlpar = "a"),
#                   #           prior(normal(0.6, 0.3), nlpar = "b"),
#                   #           prior(normal(0, 0.5), nlpar = "a", coef = "b_fac0.85"),
#                   #           prior(normal(0, 0.5), nlpar = "b", coef = "b_fac0.85")),
#                   chains = 1,
#                   iter = 1000)
# 
# brm_lm = brm(logy ~ logx*b_fac,
#              data = sim_group,
#              family = gaussian(),
#              chains = 1, 
#              iter = 1000)


# test = brm(bf(y ~ 1 + logx*b_fac),
#                   data = sim_group,
#                   family = lognormal(link = "identity"),
#                   # prior = c(prior(normal(1, 0.2), nlpar = "a"),
#                   #           prior(normal(0.6, 0.3), nlpar = "b"),
#                   #           prior(normal(0, 0.5), nlpar = "a", coef = "b_fac0.85"),
#                   #           prior(normal(0, 0.5), nlpar = "b", coef = "b_fac0.85")),
#                   chains = 1,
#                   iter = 1000)


test1 = update(test, formula = . ~ 1 + logx*b_fac, newdata = sim_group)

brm_lm = update(brm_lm, newdata = sim_group)

fit_sims1 = update(fit_sims[[1]], newdata = sim_group,
                   formula = bf(y ~ a * x^b,
                                a ~ b_fac,
                                b ~ b_fac, 
                                nl = T))

pp_check(brm_lm)
pp_check(test1)
pp_check(fit_sims1)
pp_check(brm_lm, type = "boxplot")
pp_check(test1, type = "boxplot")
pp_check(fit_sims1, type = "boxplot")

# compare mean resp rate

gaus_mean = brm_lm$data %>% 
  distinct(logx, b_fac) %>% 
  add_epred_draws(brm_lm) %>% 
  mutate(.epred = exp(.epred)) %>% 
  mutate(model = "gaussian")

lognorm_mean = test1$data %>% 
  distinct(logx, b_fac) %>% 
  add_epred_draws(test1) %>% 
  mutate(model = "lognormal")

gamma_mean = fit_sims1$data %>% 
  distinct(x, b_fac) %>% 
  add_epred_draws(fit_sims1) %>% 
  mutate(model = "gamma")


bind_rows(gaus_mean, lognorm_mean, gamma_mean) %>% 
  group_by(b_fac, model) %>% 
  reframe(mean = mean(.epred),
          sd = sd(.epred)) %>% 
  ggplot(aes(x = b_fac, y = mean, color = model)) + 
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(width = 0.2))


# compare mean diff resp rate

gaus_mean = sim_group %>% 
  distinct(logx, b_fac, x) %>% 
  add_epred_draws(brm_lm) %>% 
  mutate(.epred = exp(.epred)) %>% 
  mutate(model = "gaussian")

lognorm_mean = sim_group %>% 
  distinct(logx, b_fac, x) %>% 
  add_epred_draws(test1) %>% 
  mutate(model = "lognormal")

gamma_mean = fit_sims1$data %>% 
  distinct(x, b_fac) %>% 
  add_epred_draws(fit_sims1) %>% 
  mutate(model = "gamma")


bind_rows(gaus_mean, lognorm_mean, gamma_mean) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -logx) %>% 
  pivot_wider(names_from = b_fac, values_from = .epred) %>% 
  mutate(diff = `0.65` - `0.35`) %>% 
  group_by(model) %>% 
  reframe(mean = mean(diff),
          sd = sd(diff)) %>% 
  ggplot(aes(x = model, y = mean, color = model)) + 
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(width = 0.2))


xpreds = tibble(x = seq(min(sim_group$x),
             max(sim_group$x),
             length.out = 20),
             logx = log(x)) %>% 
  expand_grid(b_fac = unique(sim_group$b_fac))

lm_preds = xpreds %>% 
  add_epred_draws(brm_lm)

lognorm_preds = xpreds %>% 
  add_epred_draws(test1)


lognorm_med = lognorm_preds %>% 
  group_by(b_fac, logx) %>% 
  median_qi(.epred) %>% 
  mutate(model = "lognormal")

lm_med = lm_preds %>% 
  group_by(b_fac, logx) %>%
  mutate(.epred = exp(.epred)) %>% 
  median_qi(.epred) %>% 
  mutate(model = "gaussian")


bind_rows(lognorm_med, 
          lm_med) %>% 
  ggplot(aes(x = logx, y = .epred, fill = model, color = model)) + 
  geom_line() + 
  geom_point(alpha = 0.5) +
  # geom_ribbon(aes(ymin = .lower, ymax = .upper)) + 
  facet_wrap(~b_fac, scales = "free") +
  scale_y_log10()


bind_rows(lognorm_med, 
          lm_med) %>% 
  select(logx, .upper, model, b_fac) %>% 
  pivot_wider(names_from = model, values_from = .upper) %>% 
  mutate(diff = lognormal - gaussian) %>% 
  ggplot(aes(x = logx, y = diff, color = b_fac)) +
  geom_point()





