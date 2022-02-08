rm(list=ls())

library(brms)
library(tidyverse)
library(bayesplot)
library(tidybayes)

n <- 9
z <- 6
trial_data <- rep(0:1, times = c(n - z, z))
# save trial data as a tibble
trial_data <- tibble(y = trial_data)

# when you want to enter variables into the parameters defining priors in brms::brm()
# you need to specify them using the stanvar() function. Since we want to do this for
# two variables, use stanvar() twice and save the results as as object (stanvars).

omega <- .75
kappa <- 12
stanvars <- 
  stanvar(omega * (kappa - 2) + 1, name = "my_alpha") +
  stanvar((1 - omega) * (kappa - 2) + 1, name = "my_beta")

# ok, let's fit the first model, where omega = .75

fit1 <-
  brm(data = trial_data,
      family = bernoulli(link=identity),
      y ~ 1,
      prior(beta(my_alpha, my_beta), class = Intercept),
      iter = 11000, warmup = 1000, chains = 4, cores = 1,
      seed = 10,
      stanvars = stanvars,
      control = list(adapt_delta = .999))
theta <- posterior_samples(fit1)
head(theta)
fixef(fit1)
(mean_theta <- fixef(fit1)[1])
(sd_theta <- fixef(fit1)[2])
# convert to alpha and beta parameters
a_post <- mean_theta * (mean_theta * (1 - mean_theta) / sd_theta^2 - 1)
b_post <- (1 - mean_theta) * (mean_theta * (1 - mean_theta) / sd_theta^2 - 1)

# we are saving the values for n, z, omega, and kappa so that we can compute
# p(D), the probability of the data (i.e., the marginal likelihood), given the 
# given the model. The intermediary step will be computing its reciprocal, 1/p(D).

one_over_pd <- function(theta) {
  mean(dbeta(theta, a_post, b_post) /
         (theta^z * (1 - theta)^(n - z) *
            dbeta(theta, omega * (kappa - 2) + 1, (1 - omega) * (kappa -2) + 1)))
}

theta %>% 
  summarise(pd = 1/ one_over_pd(theta = b_Intercept))

# ok, now let's try the other model
omega <- .25

stanvars <- 
  stanvar(omega * (kappa - 2) + 1, name = "my_alpha") +
  stanvar((1 - omega) * (kappa - 2) + 1, name = "my_beta")

# fit the other model
fit2 <-
  brm(data = trial_data,
      family = bernoulli(link=identity),
      y ~ 1,
      prior(beta(my_alpha, my_beta), class = Intercept),
      iter = 11000, warmup = 1000, chains = 4, cores = 1,
      seed = 10,
      stanvars = stanvars,
      control = list(adapt_delta = .999))

theta <- posterior_samples(fit2)
(mean_theta <- fixef(fit2)[1])
(sd_theta <- fixef(fit2)[2])
# convert to alpha and beta parameters
a_post <- mean_theta * (mean_theta * (1 - mean_theta) / sd_theta^2 - 1)
b_post <- (1 - mean_theta) * (mean_theta * (1 - mean_theta) / sd_theta^2 - 1)

theta %>% 
  summarise(pd = 1/ one_over_pd(theta = b_Intercept))

# Hierarchical MCMC computation of relative model probability
waic(fit1)

# recommended workflow for information criteria with brms model 
# is to use the add_criterion() function

fit1 <- add_criterion(fit1, criterion = c("loo", "waic"))
fit2 <- add_criterion(fit2, criterion = c("loo", "waic"))

loo_compare(fit1, fit2)
(mw <- model_weights(fit1, fit2))
mw[1]/mw[2]

## autocorrelation and effect size
mcmc_acf(posterior_samples(fit1, add_chain = T),
         pars = "b_Intercept",
         lags = 35)
neff_ratio(fit1)[1] %>% 
  mcmc_neff() +
  yaxis_text(hjust = 0)
rhat(fit1[1])

# models with different "noise" distributions in brms
n <- 1e3
set.seed(10)
(d <- tibble(y=rt(n, df=7)))

d %>% 
  ggplot(aes(x=y)) +
  geom_histogram(color = "grey92", fill = "grey67",
                 size = .2, bins = 30) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank())

fit3 <-
  brm(data = d,
      family = gaussian,
      y~1,
      prior = c(prior(normal(0,5), class = Intercept),
                prior(normal(0,5), class = sigma)),
      chains = 4, cores = 1,
      seed = 10)

fit4 <-
  brm(data = d,
      family = student,
      y ~ 1,
      prior = c(prior(normal(0,5), class = Intercept),
                prior(normal(0,5), class = sigma),
                prior(gamma(2, 0.1), class = nu)),
      chains = 4, cores = 1,
      seed = 10)

posterior_summary(fit3) %>% round(digits = 2)
posterior_summary(fit4) %>% round(digits = 2)

fit3 <- add_criterion(fit3, criterion = c("loo", "waic"))
fit4 <- add_criterion(fit4, criterion = c("loo", "waic"))

loo_compare(fit3, fit4)
model_weights(fit3, fit4)
pp_check(fit3)
pp_check(fit4)

## model averaging
posterior_samples(fit1) %>% 
  ggplot(aes(x = b_Intercept)) +
  geom_histogram(color = "grey92", fill = "grey67",
                 size = .2, bins = 30) +
  stat_pointinterval(aes(y=0),
                     point_interval = mode_hdi, .width = c(.95, .5)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "The posterior for the probability, given fit1",
       x = expression(paste(italic(p), "(", theta, "|", italic(D), ", ", omega, " =.75)"))) +
  coord_cartesian(xlim = 0:1) +
  theme(panel.grid = element_blank())

nd <- tibble(y=1)

pp_a <-
  pp_average(fit1, fit2,
             newdata = nd,
             weights = "stacking",
             method = "fitted",
             summary = F) %>% 
  as_tibble() %>% 
  set_names("theta")
head(pp_a)

pp_a %>% 
  ggplot(aes(x = theta)) +
  geom_histogram(color = "grey92", fill = "grey67", 
                 size = .2, bins = 30) +
  stat_pointinterval(aes(y=0),
                     point_interval = mode_hdi, .width = c(.95, .5)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "The posterior for the probability, given the\nweighted combination of fit1 and fit2",
       x = expression(paste(italic(p), "(", theta, "|", italic(D), ")"))) +
  coord_cartesian(xlim = 0:1) +
  theme(panel.grid = element_blank())

# 10.5 Model complexity naturally accounted for

# how granular do you want the theta sequence
n <- 1e3

# simulate the data
tibble(omega = .5,
       kappa = c(1000, 2),
       model = c("The must-be-fair model", "The anything's-possible model")) %>% 
  expand(nesting(omega, kappa, model),
         theta = seq(from = 0, to = 1, length.out = n)) %>% 
  mutate(density = dbeta(theta,
                         shape1 = omega  * (kappa - 2) + 1,
                         shape2 = (1 - omega) * (kappa - 2) + 1)) %>% 
  #plot
  ggplot(aes(x=theta, ymin =0, ymax = density)) +
  geom_ribbon(fill = "grey67") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Note that in this case, their y-axes are on the same scale.",
       x = expression(theta)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~model)

tibble(omega = .5,
       kappa = c(1000, 2),
       model = c("The must-be-fair model", "The anything's-possible model")) %>%
  mutate(alpha =      omega  * (kappa - 2) + 1,
         beta  = (1 - omega) * (kappa - 2) + 1)


# the data summaries
z <- 15
n <- 20

p_d <- function(z, n, a, b) { 
  beta(z + a, n - z + b) / beta(a, b) 
}

p_d(z, n, a = 500, b = 500) / p_d(z, n, a = 1, b = 1)

z <- 11
p_d(z, n, a = 500, b = 500) / p_d(z, n, a = 1, b = 1)

# caveats regarding nested model comparison

my_data <- read_csv("/Users/tua37526/Dropbox/BayesTutorial/DBDA2Eprograms/BattingAverage.csv")
glimpse(my_data)

fit5 <-
  brm(data = my_data,
      family = binomial(link = logit),
      Hits | trials(AtBats) ~ 1 + (1 | PriPos) + (1 | PriPos:Player),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0,1), class = sd)),
      iter = 3500, warmup = 500, chains = 3, cores = 1,
      control = list(adapt_delta = .99),
      seed = 10)

my_data <-
  my_data %>% 
  mutate(PriPos_small = if_else(PriPos %in% c("Center Field", "Left Field", "Right Field"),
                                "Outfield", PriPos))

fit6 <-
  update(fit5,
         newdata = my_data,
         formula = Hits | trials(AtBats) ~ 1 + (1|PriPos_small) + (1|PriPos_small:Player),
         iter = 3500, warmup = 500, chains = 3, cores = 1,
         control = list(adapt_delta = .99),
         seed = 10)

