format_data_for_maggi <- function(nsub=1,nses=1,ncontext=1,specific_doors=FALSE){

# lydia barnes, may 2024
# reads event data from doors task
# re-codes accuracy so that "success" trials (<=4 clicks) that inc. context-irrelevant doors are counted as failures
# classifies trials by whether they provide evidence for learning 1, 2, 3, or 4 doors

# sources
library(tidyverse)

# event data
events <- read.csv('res/study-01_exp_lt_clicks_evt.csv')
events <- events %>% filter(sub==nsub, ses==nses, context==ncontext)

# find the point in each trial where they made their first mistake
first_errors <- rep(0,1,max(unique(events$t)))
for (i in unique(events$t)){
  tdata <- events %>% filter(t==i)
  try(first_errors[i] <- min(which(diff(tdata$door_cc)==-1)), silent = TRUE)
}
ttrials <- unique(events$t)

# find the first trial in which each door is the target (i.e. people's first opportunity to learn)
doors <- events %>% filter(door_cc==1) %>% pull(door) %>% unique()
target_idx <- c(which(diff(events$t)==1),length(events$door))
targets <- events$door[target_idx]
first_feedback <- rep(0,1,length(doors))
for (i in 1:length(doors)){
  first_feedback[i] <- events$t[target_idx[min(which(targets==doors[i]))]]
}

# exclude trials w <4 clicks on which all doors were context-relevant
uninformative <- events %>% group_by(t) %>% summarise(clicks = n(),accuracy = mean(door_cc)) %>% 
  mutate(exclude = case_when((accuracy==1 & clicks<4) ~ t,.default=0)) %>% filter(exclude!=0) %>% pull(exclude)
events_include <- events %>% filter(!(t %in% uninformative))

# preallocate arrays
trials <- unique(events_include$t)
ntrials <- length(trials)
nstrategies <- 4
learn_doors <- matrix(0,nstrategies,ntrials)

# for remaining trials, 
for (i in 1:ntrials){
  trial <- trials[i]
  tdata <- events_include %>% filter(t==trial)
  
  # if they haven't had a chance to experience a door as a target, treat it as context-irrelevant
  if((nses==1) &(trial < max(first_feedback))){
    chance_doors <- doors[which(first_feedback>=trial)]
    tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc))
  }
  
  # if the first door clicked is context-irrelevant, count that as evidence against learning
  if(tdata$door_cc[1]==0){
    learn_doors[,i] <- 0
  }else{
    
    # if we couldn't find their first mistake, they've learned all four doors
    if(is.infinite(first_errors[trial])){
      learn_doors[,i] <- 1 
      
    # if we've found their first mistake, look at the correct doors they found before the mistake
    }else{
      these_doors <- unique(tdata$door[1:first_errors[trial]])
      
      # if we're requiring that they prioritise the same subset of doors before we consider clicks deliberate,
      if(specific_doors){
        
        door_idx <- match(these_doors,doors)
        learn_doors[door_idx,i] <- 1
        
      # if we're allowing that people may click different subsets of doors, but must be consistent with the previous trial,
      }else{
        if(i==1){
          learn_doors[,i] <- 0
        }else{
          # find out what doors they selected on the previous trial
          idx <- which(ttrials==trial)
          those_doors <- events %>% filter(t==ttrials[idx-1]) %>% pull(door) %>% unique()
          
          # if there are errors on the previous trial, take the doors before the error
          if(!is.infinite(first_errors[trials[i-1]])){
            those_doors <- unique(those_doors[1:first_errors[trials[i-1]]])
          }
          
          # if any correct doors were prioritised on both trials, count that as evidence of learning however many doors 
          # have stayed the same
          if(any(!is.na(match(these_doors,those_doors)))){
            learn_doors[1:length(which(!is.na(match(these_doors,those_doors)))),i] <- 1
          }
        }
      }
    }
  }
}

# if we're requiring that they continuing selecting e.g. door 11 as their learn_one door, 
if(specific_doors){
  # find the order in which they first selected the doors
  first_selection <- rep(0,nstrategies)
  for (i in 1:nstrategies){
    first_selection[i] <- min(which(learn_doors[i,]==1))
  }
  # exclude trials on which previously-selected doors aren't among those selected
  for (i in 1:ntrials){
    for (j in 1:nstrategies){
      if(trials[i]>first_selection[j] & learn_doors[j,i]==0){
        learn_doors[,i] <- 0
      }
    }
  }  
  # sort by which doors were selected first
  learn_doors <- learn_doors[order(first_selection),]
}

# format
strategies <- data.frame(trials,learn_one=learn_doors[1,],learn_two=learn_doors[2,],learn_three=learn_doors[3,],learn_four=learn_doors[4,])
return(strategies)

}