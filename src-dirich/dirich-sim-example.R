## K. Garner, 2024
### simulate data from a Dirichlet distribution and recover the parameters, 
## plus compute entropy
## this code is meant to serve as a basis for a script to run over participants,
## as the coding great Lydia Barnes sees fit
rm(list=ls())

###### some helpful things
library(withr)
library(tidyverse)
library(rstan) # installation guide here - https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(brms) # use regular install.packages()
library(tidybayes)
library(wesanderson)

##########
# extra info:
# https://www.andrewheiss.com/blog/2023/09/18/understanding-dirichlet-beta-intuition/
##########

####### functions


############################################
## step 1. use a dirichlet distribution to pick from the six possible routines
## at chance
n_routines = 6 # there are 6 possible routines
alpha_k <- rep(1, times=n_routines) # shape parameter, like the beta, uniform priors
n_observations <- 160 # assuming the total counts is 160
# distribution counts, but for each possible routine
sim_data <- with_seed(42, {
  round(rdirichlet(n=1, alpha_k)*n_observations, 0) |>
    data.frame() |>
    set_names(paste("r", 1:n_routines, sep="")) |>
    pivot_longer(everything(), names_to="routine", values_to="counts")
})


############################################
## step 2
# first thing I want to do is compute the alphas for the
# dirichlet distribution, and visualise the results as a beta
# distribution for each alpha parameter
sim_data <- sim_data %>% mutate(posterior_alphas = counts + 1)

# the most likely theta parameters for this Dirchlet distribution
# is simply the expected value for each routine
sum_counts <- with(sim_data, sum(posterior_alphas))
sim_data <- sim_data %>% mutate(theta=posterior_alphas / sum_counts)
with(sim_data, sum(theta)) == 1 # should be TRUE

# now I visualise
viz_dir <- 
  
  
  
  ggplot(aes(x=theta, fill=routine)) +
    geom_density(bounds=c(0,1), colour=NA) +
    scale_x_continuous(breaks = seq(0,1, by=0.2)) +
    scale_fill_manual(wes_palette("IsleofDogs1")) +
    labs(x = "p")
  
# now what I want to do is plot the separate beta distributions for
# each alpha, and we can work out which ones share 95% CIs with

# now I want to obtain the maximum posterior probability for each routine, for
# that participant, and the probability that any given alpha 
# contains the null hypothesis



# and now I compute the entropy, to give me a nice measure of how dispersed or
# clustered the individual's selection of routines was
