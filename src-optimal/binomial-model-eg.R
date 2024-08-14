### K. Garner, 20024
### this code shows how to run a bayesian binomial model on some dummy
### data, assuming the data has been coded into n & m states

### -----------------------------------------------------------------------
# resources
# McElreath (2020). Rethinking Statistics. Chap 8: God Spiked the Integers

rm(list=ls())
### -----------------------------------------------------------------------
# helpful things
library(withr)
library(tidyverse)
library(rstan)
library(brms)

### -----------------------------------------------------------------------
# 1. generate some dummy data
# first I'll load our predictors, which I saved from the 
# 'bayes-compute-context-probs' doc
load("src-optimal/posterior-odds.Rda")
# first, its not sensible to think about 4 separate null selections
# from n, as the fourth has to hit the target, so removing that from the 
# dataframe, also removing instances when m = 4, as the actor would have 
# certainty about context at that point
odds <- odds %>% filter(n != 4) %>% filter(m != 4)

# now I am going to make this dataframe wider, to that I can more easily
# generate some fake data
odds <- odds %>% pivot_wider(names_from = condition, values_from = odds)

# note that because 'success|context' is a single constant,
# this is akin to saying that the data can be modelled using only a grand mean
# and a subject intercept, so going to drop that from the
# dataframe
odds[,"success|context"] <- NULL

# now I am going to assume 100 subjects, and simulate the selection of m door
# contexts, given a state of n and m
# I am going to assume a main effect for the context regressor, a subject intercept
# and random error
generate_count_dat_one_sub <- function(odds, i){
  # define main effects
  a <- -0.2 # intercept
  b_c <- 0.25 # beta context

  # other useful things
  total_states <- nrow(odds)
  n_trials <- 160 # we have 160 trials, and 16 states, so I am going
  context <- odds$context
  # to assume the total visits to each state is the EV, give or take a 
  # little bit
  total_times_in_each_state <- round(rnorm(total_states, 
                                           mean=n_trials/total_states,
                                           sd=1))
  # calculate the p for each state
  log_odds <- a + b_c*context + rnorm(1, mean=0, sd=2) + rnorm(length(context)) # note 
  # exactly how you generate the log odds will depend on the model you are testing
  # e.g. I changed it when testing the hierarchal model
  p <- 1/(1+exp(-log_odds))
  # for each p, draw from the binomal distribution
  resp <- unlist(lapply(p, rbinom, n=total_times_in_each_state, size=1))
  context <- rep(context, each=length(p))
  
  data <- data.frame(sub = i,
                     context = context,
                     resp = resp)
  data
}

# now I'll generate data for 100 subs
dummy_data <- do.call(rbind, lapply(1:100, generate_count_dat_one_sub, odds=odds))
# to scale or not to scale context? good discussion here
# https://discourse.mc-stan.org/t/re-scaling-data-and-parameters-to-0-1/16275/4
# leaving for now
#dummy_data$test <- scale(dummy_data$context)

### -----------------------------------------------------------------------
# 2. setting up the model
# first lets set up a model that contains only the subject intercept and the 
# grand intercept
source('src-optimal/stan-binomial-model-grandintercept-only.R') # load the model string

# step 1 is to make the variables that will be fed into the stan model
# note I am making more variables than strictly needed, thats for the next stage
ndata <- nrow(dummy_data) # n of observations
nsubs <- length(unique(dummy_data$sub))
context <- dummy_data$context
sub <- dummy_data$sub
resp <- dummy_data$resp

# make a nice data list for stan
data_list <- list(ndata = ndata, context = context,
                   resp=resp, nsubs = nsubs, sub = sub)


### -----------------------------------------------------------------------
# 3. Running the model 
# the short, hopefully simple bit
int_model <- stan(model_code = get("int_only_model"),
                   data = data_list,
                   warmup = 1000,
                   iter = 2000,
                   chains = 4)

### -----------------------------------------------------------------------
# 4. Model checks
# step 1, always, check the chains have converged
traceplot(int_model, pars=c("beta0")) # should show nice hairy caterpillars

# now check that Rhat is close to 1 and ESS is a reasonable number, for the
# key parameters
print(int_model)

