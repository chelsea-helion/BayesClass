library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(skimr)

## bayes homework for HLM
## some code adapted from: https://bookdown.org/ajkurz/DBDA_recoded/hierarchical-models.html#extending-the-hierarchy-subjects-within-categories
## load in SC decision data
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

## combine across votes based on justice
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

make_histogram <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = "grey67", color = "grey92", size = .2,
                   bins = 30) +
    stat_pointintervalh(aes(y = 0), 
                        point_interval = mode_hdi, .width = .95) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}



fit2 <- 
  brm(data = newDat3,
      family = binomial(link = logit),
      sum_agree | trials(sum_votes) ~ 1 + (1|Party) + (1|Party:justiceName),
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = sd)),
      iter = 11000, warmup = 1000, chains = 1,
      control = list(adapt_delta = .999, max_treedepth = 15), 
      seed = 9)
plot(fit2)
print(fit2)
pairs(fit1)



