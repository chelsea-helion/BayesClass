library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(skimr)

## bayes homework for HLM
## load in SC decision data
SCdat <- read_csv("/Users/tua37526/Downloads/SCDB_2021_01_justiceCentered_Citation.csv")
skim(SCdat)
## load in political party data
Pdat <- read_csv("/Users/tua37526/Downloads/Justices.csv")
skim(Pdat)

## filter columns of interest
newDat <- SCdat %>% 
  select(justiceName, vote, direction) %>% 
  filter(vote == 1 | vote == 2)

## check that the names match
all(newDat$justiceName %in% Pdat$justiceName)

## move party into the new dataframe
newDat <- inner_join(newDat, Pdat)

## combine across votes based on justice



## fit bayesian model
fit1 <- 
  brm(data = newDat,
      family = binomial(link = logit),
      majVotes | trials(vote) ~ 1 + (1|Party) + (1|Party:justiceName),
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = sd)),
      iter = 3500, warmup = 500, chains = 3, cores = 3,
      control = list(adapt_delta = .99), 
      seed = 9)

color_scheme_set("blue")
plot(fit2)




