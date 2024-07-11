# lydia barnes, may 2024
# applies maggi algorithm. algorithm estimates probability of using a given strategy, weighting by recency.

library(zeallot) #unpack/destructure with %<-%
library(tidyverse)

source(file.path(getwd(), "src-learn", "get_maggi.R"))
source(file.path(getwd(), "src-learn", "format_data_for_maggi.R"))
source(file.path(getwd(), "src", "get_subs.R"))

set.seed(17)
simulation <- FALSE

if (simulation){
  # synthesise data
  ps <- c(.1, .3, .6, .9) #reasonable steps in the probability of successes over runs of trials
  n <- 25 #trials per run
  data <- unlist(lapply(ps, rbinom, n=n, size=1)) # n * length(ps) trials drawn from a binomial distribution
  c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)
  
  # view the final beta distribution
  increments <- seq(0,1,by=.01)
  plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 
  
}else{
  # read real data
  
  # settings ----------------------------------------------------------------
  version <- "study-01" # pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
  exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
  ses <- 3 # session: 1 = 'ses-learn', 2 = 'ses-train', 3 = 'ses-test'.
  conditions <- c(1,2) # if ses = 3: 1 = complete transfer, 2 = partial transfer. if ses = 1: 1 = context 1, 2 = context 2
  project_path <- getwd()
  subs <- get_subs(exp, version)
  colours <- c("darkgreen","limegreen","gold","orange")
  view_all <- FALSE

  events <- read.csv('res/study-01_exp_lt_clicks_evt.csv')
  
  group_data <- data.frame(
    sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), k4_onset = integer(), nclicks = integer()
  )
  
  for (sub in subs){
    
    print(sub)
    sid <- as.numeric(substring(sub,5,7))

    train_type <- events %>% filter(sub==sid, ses==2) %>% pull(train_type)
    train_type <- train_type[[1]]
    
    for (condition in conditions){
      
      # evidence ----------------------------------------------------------------
      strategies <- format_data_for_maggi(nsub=sid,nses=ses,ncontext=condition,method="by_event",specific_doors=FALSE,competitive=TRUE,evaluate_all=FALSE)
    
      # maggi -------------------------------------------------------------------
      if (view_all){
        
        # empty figure ------------------------------------------------------------
        plot(1:nrow(strategies),rep(0,1,nrow(strategies)),type="l",col="black",ylim=c(0,1))
        
        i <- 0
        for (strategy in names(strategies)[2:length(names(strategies))]){
          i <- i+1
          strategy <- strategies %>% pull(strategy)
          
          # calculate recency-weighted probability of finding strategy s
          c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(strategy)
          
          # view alphas and betas over time
          points(1:length(strategy),beta_map,type="l",col=colours[i])
          
        }
      } else {
        k4 <- strategies %>% pull(k4)
        c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(k4)
      }
  
      # threshold ---------------------------------------------------------------
      
      # select the trial at which evidence for K4 first exceeds a threshold
      k4_onset <- min(which(beta_map > .5))
      if(k4_onset == Inf){k4_onset <- NA}
      
      nclicks <- length(beta_map)
      
      sub <- sid
      if (ses < 3){
        context <- condition
        transfer <- NA
      }else{
        context <- NA
        transfer <- condition
      }
      data <- data.frame(sub,ses,context,transfer,train_type,k4_onset,nclicks)
      
      group_data <- rbind(group_data,data)
    }
  }
}

write.csv(group_data,'res/study-01_exp_lt_ses-test_k4.csv')
print('')

