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

newDat$justiceName %in% Pdat$justiceName
