# lydia barnes, may 2024

library(zeallot) #unpack/destructure with %<-%
source(file.path(getwd(), "src-learn", "get_maggi.R"))

# synthesise data
ps <- c(.1, .3, .6, .9) #reasonable steps in the probability of successes over runs of trials
n <- 25 #trials per run
data <- unlist(lapply(ps, rbinom, n=n, size=1)) # n * length(ps) trials drawn from a binomial distribution

# calculate recency-weighted probability of finding strategy s
c(alphas,betas,beta_mean,beta_variance) %<-% get_maggi(data)

# compare to output from maggi's python code
data_from_maggi <- read.csv('../maggi_toolbox/test_output.csv')

plot(1:length(beta_variance),1/beta_variance,type="l",col="navy")
points(1:length(data_from_maggi$Precision),data_from_maggi$Precision,type="l",col="darkorange")

plot(1:length(data),alphas,type="l",col="darkgreen")
points(1:length(data),betas,type="l",col="navy")
points(1:length(data_from_maggi$Alpha),data_from_maggi$Alpha,type="l",col="pink")
points(1:length(data_from_maggi$Beta),data_from_maggi$Beta,type="l",col="darkred")

increments <- seq(0,1,by=.01)
plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 
points(increments,dbeta(increments,data_from_maggi$Alpha[length(data_from_maggi$Alpha)],data_from_maggi$Beta[length(data_from_maggi$Beta)]),type="l",col="orange")

plot(1:length(data),beta_mean,type="l",col="darkgreen")
plot(1:length(data),beta_variance,type="l",col="darkblue")

