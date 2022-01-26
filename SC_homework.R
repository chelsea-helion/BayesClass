library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(skimr)
library(patchwork)

## bayes homework for HLM
## some code adapted from: https://bookdown.org/ajkurz/DBDA_recoded/hierarchical-models.html#extending-the-hierarchy-subjects-within-categories
## load in SC decision data (from: http://scdb.wustl.edu/data.php)
SCdat <- read_csv("/Users/tua37526/Downloads/SCDB_2021_01_justiceCentered_Citation.csv")
skim(SCdat)
## load in political party data
Pdat <- read_csv("/Users/tua37526/Downloads/Justices.csv")
skim(Pdat)

## filter columns of interest, recode 2 (dissent) to 0 (1 = voted with majority)
newDat <- SCdat %>% 
  select(justiceName, vote, direction) %>% 
  filter(vote == 1 | vote == 2) %>% 
  mutate(vote_recode = ifelse(vote == 2, 0, 1))


## check that the names match
all(newDat$justiceName %in% Pdat$justiceName)

## combine across votes based on justice -- overall tendency to agree
newDat2 <- newDat %>% 
  group_by(justiceName) %>%
  summarise(N = n(), 
            sum_votes = length(vote_recode), 
            sum_dissent = length(vote_recode[vote == 2]),
            sum_agree = length(vote_recode[vote == 1]))

## move party into the new dataframe
newDat3 <- inner_join(newDat2, Pdat)


## fit bayesian model
fit1 <- 
  brm(data = newDat3,
      family = binomial(link = logit),
      sum_agree | trials(sum_votes) ~ 1 + (1|Party) + (1|Party:justiceName),
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = sd)),
      iter = 4000, warmup = 1500, chains = 3, cores = 3,
      control = list(adapt_delta = .999, max_treedepth = 15), 
      seed = 9)
plot(fit1)
print(fit1)
pairs(fit1)
post <- posterior_samples(fit1, add_chain = T)
mcmc_acf(post, pars = c("b_Intercept", 
                        "sd_Party__Intercept",
                        "sd_Party:justiceName__Intercept"), lags = 8)

## kind of a bad Neff ratio, but it is what it is
fit1 %>% 
  neff_ratio() %>% 
  mcmc_neff_hist(binwidth = .1) +
  yaxis_text()

## get group-level parameters for hierarchical model
post <-
  post %>% 
  as_tibble()
head(post)

## convert to probability space
post_s <-
  post %>% 
  transmute(Democrat = (b_Intercept +`r_Party[D,Intercept]`),
            Republican = (b_Intercept +`r_Party[R,Intercept]`)) %>% 
  mutate_all(inv_logit_scaled) %>% 
  mutate(`Democrat - Republican` = Democrat - Republican)
head(post_s)

## graphing functions
## copied from: https://bookdown.org/ajkurz/DBDA_recoded/hierarchical-models.html#extending-the-hierarchy-subjects-within-categories
make_histogram <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = "grey67", color = "grey92", size = .2,
                   bins = 30) +
    stat_pointinterval(aes(y = 0), 
                        point_interval = mode_hdi, .width = .95) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}

make_point <- function(data, mapping, limits, ...) {
  
  ggplot(data, mapping) +
    geom_abline(color = "white") +
    geom_point(color = "grey50", size = 1/10, alpha = 1/20) +
    coord_cartesian(xlim = limits,
                    ylim = limits)
  
}

p1 <-
  make_histogram(data = post_s,
                 aes(x = Democrat), 
                 title = "Democrat", 
                 xlim = c(0.7, 1))
p1

p2 <-
  make_histogram(data = post_s,
                 aes(x = `Democrat - Republican`), 
                 title = "Democrat - Republican", 
                 xlim = c(-.1, .07))
p2

p3 <-
  make_point(data = post_s,
             aes(x = Democrat, y = Republican),
             limits = c(.75, .90))
p3

p4 <-
  make_histogram(data = post_s,
                 aes(x = Republican), 
                 title = "Republican", 
                 xlim = c(.7, 1))
p4

p1 + p2 + p3 + p4

## combine across votes based on justice -- overall tendency to agree based on
## direction of the decision (1 = conservative, 2 = liberal)
newDat4 <- newDat %>% 
  group_by(justiceName, direction) %>%
  summarise(N = n(), 
            sum_votes = length(vote_recode), 
            sum_dissent = length(vote_recode[vote == 2]),
            sum_agree = length(vote_recode[vote == 1]))
newDat4 <- newDat4 %>% 
  filter(!is.na(direction))

## move party into the new dataframe
newDat4 <- inner_join(newDat4, Pdat)


## fit bayesian model
fit2 <- 
  brm(data = newDat4,
      family = binomial(link = logit),
      sum_agree | trials(sum_votes) ~ 1 + direction:Party + (1|Party) + (1|Party:justiceName),
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = sd)),
      iter = 4000, warmup = 1500, chains = 3, cores = 3,
      control = list(adapt_delta = .99, max_treedepth = 15),
      seed = 9)
plot(fit2)
print(fit2)
pairs(fit2)
summary(fit2)$fixed
summary(fit2)$random
post2 <- posterior_samples(fit2, add_chain = T)
mcmc_acf(post2, pars = c("b_Intercept",
                        "b_direction:PartyD",
                        "b_direction:PartyR",
                        "sd_Party__Intercept",
                        "sd_Party:justiceName__Intercept"), lags = 8)

## kind of a bad Neff ratio, but better than the last one (shrug emoji)
fit2 %>% 
  neff_ratio() %>% 
  mcmc_neff_hist(binwidth = .1) +
  yaxis_text()

## get group-level parameters for hierarchical model
post2 <-
  post2 %>% 
  as_tibble()
head(post2)

## convert to probability space
post2_s <-
  post2 %>% 
  transmute(Democrat = (b_Intercept +`r_Party[D,Intercept]` + `b_direction:PartyD`),
            Republican = (b_Intercept +`r_Party[R,Intercept]` + `b_direction:PartyR`)) %>% 
  mutate_all(inv_logit_scaled) %>% 
  mutate(`Democrat - Republican` = Democrat - Republican)
head(post2_s)


## pick two justices
name_list <- c("AMKennedy", "JGRoberts")
nd_cons <-
  newDat4 %>% 
  filter(justiceName %in% name_list) %>% 
  filter(direction == 1)
nd_lib <-
  newDat4 %>% 
  filter(justiceName %in% name_list) %>% 
  filter(direction == 2)

fitted_justices_cons <-
  fitted(fit2, 
         newdata = nd_cons,
         scale = "linear",
         summary = F) %>% 
  as_tibble() %>% 
  # rename the values as returned by `as_tibble()`
   set_names(name_list) %>% 
  # convert the values from the logit scale to the probability scale
  mutate_all(inv_logit_scaled) %>% 
  # in this last section, we make our difference distributions 
  mutate(`AMKennedy - JGRoberts` = `AMKennedy` - `JGRoberts`)
glimpse(fitted_justices)

fitted_justices_lib <-
  fitted(fit2, 
         newdata = nd_lib,
         scale = "linear",
         summary = F) %>% 
  as_tibble() %>% 
  # rename the values as returned by `as_tibble()`
  set_names(name_list) %>% 
  # convert the values from the logit scale to the probability scale
  mutate_all(inv_logit_scaled) %>% 
  # in this last section, we make our difference distributions 
  mutate(`AMKennedy - JGRoberts` = `AMKennedy` - `JGRoberts`)
glimpse(fitted_justices_lib)

p1 <-
  make_histogram(data = fitted_justices_cons,
                 aes(x = `AMKennedy - JGRoberts`), 
                 title = "AMKennedy - JGRoberts (Conservative Decisions)", 
                 xlim = c(-.05, .075))
p1

p2 <-
  make_histogram(data = fitted_justices_lib,
                 aes(x = `AMKennedy - JGRoberts`), 
                 title = "AMKennedy - JGRoberts (Liberal Decisions)", 
                 xlim = c(-.05, .075))
p2

p1+p2

