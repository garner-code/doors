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
  sess <- c(1,3) # session: 1 = 'ses-learn', 2 = 'ses-train', 3 = 'ses-test'.
  conditions <- c(1,2) # if ses = 3: 1 = complete transfer, 2 = partial transfer. if ses = 1: 1 = context 1, 2 = context 2
  project_path <- getwd()
  subs <- get_subs(exp, version)
  colours <- c("darkgreen","limegreen","gold","orange")
  save_plots <- FALSE

  events <- read.csv('res/study-01_exp_lt_clicks_evt.csv')
  
  group_data <- data.frame(
    sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), event = integer(), 
    k1 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric(), win = integer(), stable_k4 = integer()
  )
  
  for (subject in subs){
    
    print(subject)
    sid <- as.numeric(substring(subject,5,7))

    train_type <- events %>% filter(sub==sid, ses==2) %>% pull(train_type)
    train_type <- train_type[[1]]
    
    for (ses in sess){
      
      for (condition in conditions){
        
        if (ses < 3){
          context <- condition
          transfer <- NA
        }else{
          context <- NA
          transfer <- condition
        }
        
        # evidence ----------------------------------------------------------------
        strategies <- format_data_for_maggi(nsub=sid,nses=ses,ncontext=condition,method="by_event",specific_doors=FALSE,competitive=TRUE,evaluate_all=FALSE)
        
        # empty figure ------------------------------------------------------------
        if (save_plots){
          fnl <- file.path(project_path,'fig',paste(paste(version, exp, subject, ses, condition, "maggi", sep = "_"), ".png", sep = ""))
          png(file = fnl)
          plot(1:nrow(strategies),rep(0,1,nrow(strategies)),type="l",col="black",ylim=c(0,1))
        }
        
        # maggi -------------------------------------------------------------------
        i <- 0
        beta_maps <- matrix(NA,4,nrow(strategies))
        for (strategy in names(strategies)[2:length(names(strategies))]){
          i <- i+1
          strategy <- strategies %>% pull(strategy)
          
          # calculate recency-weighted probability of finding strategy s
          c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(strategy)
          
          # store data
          beta_maps[i,1:ncol(beta_maps)] <- beta_map
          
          if (save_plots){
            # view alphas and betas over time
            points(1:length(strategy),beta_map,type="l",col=colours[i])
          }
          
        }
        
        if (save_plots){
          dev.off()
        }

        # format the data
        data <- data.frame(sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), event = integer(), k1 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric(), win = integer())
        for (event in 1:length(beta_map)){
          win <- which(beta_maps[1:nrow(beta_maps),event] == max(beta_maps[1:nrow(beta_maps),event]))
          if (sum(beta_maps[1:nrow(beta_maps),event])==0){win <- NA}
          tmp <- data.frame(sid, ses, context, transfer, train_type, event, k1 = beta_maps[1,event], k2 = beta_maps[2,event], k3 = beta_maps[3,event], k4 = beta_maps[4,event], win)
          data <- rbind(data,tmp)
        }
        last_strategy_change <- max(which(diff(data$win)!=0))+1
        data <- data %>% 
          mutate(stable_k4 = case_when(event < last_strategy_change ~ 0, event %in% intersect(which(event >= last_strategy_change), which(win == 4)) ~ 1, .default = NA))
        
        group_data <- rbind(group_data,data)
        
      }
    }
  }
  
  # threshold ---------------------------------------------------------------
results <- group_data %>% group_by(sid,ses,context,train_type,transfer) %>% summarise(nclicks = n(),k4_onset = min(which(stable_k4==1)))
  
}

write.csv(group_data,file.path('res',paste(paste(version,exp,'maggi-map',sep='_'),'csv', sep='.')))
write.csv(results,file.path('res',paste(paste(version,exp,'maggi-k4',sep='_'),'csv', sep='.')))

results_wide <- results %>% filter(ses==1) %>% rename(k4_learn=k4_onset)
results_wide$k4_test <- results %>% filter(ses==3) %>% pull(k4_onset)

