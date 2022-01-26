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

## filter columns of interest, recode 2 (liberal) to 0 (1 = conservative)
newDat <- SCdat %>% 
  select(justiceName, direction) %>% 
  filter(!is.na(direction)) %>% 
  mutate(direction_recode = ifelse(direction == 2, 0, 1)) 


## check that the names match
all(newDat$justiceName %in% Pdat$justiceName)

## combine across votes based on justice -- overall tendency to agree
newDat2 <- newDat %>% 
  group_by(justiceName) %>%
  summarise(N = n(), 
            sum_votes = length(direction_recode), 
            sum_liberal = length(direction_recode[direction == 2]),
            sum_conservative = length(direction_recode[direction == 1]))

## move party into the new dataframe
newDat3 <- inner_join(newDat2, Pdat)


## fit bayesian model
fit1 <- 
  brm(data = newDat3,
      family = binomial(link = logit),
      sum_liberal | trials(sum_votes) ~ 1 + (1|Party) + (1|Party:justiceName),
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
                 xlim = c(.3,.75))
p1

p2 <-
  make_histogram(data = post_s,
                 aes(x = `Democrat - Republican`), 
                 title = "Democrat - Republican", 
                 xlim = c(-.1, .50))
p2

p3 <-
  make_point(data = post_s,
             aes(x = Democrat, y = Republican),
             limits = c(.3, .75))
p3

p4 <-
  make_histogram(data = post_s,
                 aes(x = Republican), 
                 title = "Republican", 
                 xlim = c(.3, .75))
p4

p1 + p2 + p3 + p4

## pick two justices
name_list <- c("AMKennedy", "JGRoberts")
nd <-
  newDat3 %>% 
  filter(justiceName %in% name_list)

fitted_justices <-
  fitted(fit1, 
         newdata = nd,
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

p1 <-
  make_histogram(data = fitted_justices,
                 aes(x = `AMKennedy - JGRoberts`), 
                 title = "AMKennedy - JGRoberts (Liberal Decisions)", 
                 xlim = c(-.075, .075))
p1


