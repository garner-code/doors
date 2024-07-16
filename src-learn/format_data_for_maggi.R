format_data_for_maggi <- function(nsub=1,nses=1,ncontext=1,method="by_event",specific_doors=FALSE,competitive=FALSE,evaluate_all=FALSE){

# lydia barnes, may 2024
# reads event data from doors task
# re-codes accuracy so that "success" trials (<=4 clicks) that inc. context-irrelevant doors are counted as failures
# classifies trials by whether they provide evidence for learning 1, 2, 3, or 4 doors

# sources
library(tidyverse)

# event data
events <- read.csv('res/study-01_exp_lt_clicks_evt.csv')
events <- events %>% filter(sub==nsub, ses==nses)
if (nses < 3){
  
  # only look at the first learning block for each context, not the consolidation trials
  first_attempt <- diff(events$context)
  first_attempt <- which(first_attempt==-1)
  events <- events[1:first_attempt,1:ncol(events)]
  
  events <- events %>% filter(context==ncontext)
}else{
  events <- events %>% filter(transfer==ncontext)
}

# find the first time each door is the target (their first opportunity to learn)
doors <- events %>% filter(door_cc==1) %>% pull(door) %>% unique()
target_idx <- c(which(diff(events$t)==1),length(events$door))
targets <- events$door[target_idx]
first_feedback <- rep(0,1,length(doors))
first_feedback_trial <- first_feedback
for (i in 1:length(doors)){
  first_feedback[i] <- target_idx[min(which(targets==doors[i]))]
  first_feedback_trial[i] <- events$t[first_feedback[i]]
}


# -------------------------------------------------------------------------
# analyse by event

if(method=="by_event"){
  know_doors <- matrix(0,4,nrow(events))
  for (i in 1:nrow(events)){
    j <- 3
    these_doors <- NA
    while (1){
      if(j>=i){break}
      pdata <- events[(i-j):i,]
      these_doors <- unique(pdata$door)
      j <- j+1
      if(length(these_doors)>=4){break}
    }
    correct <- which(!is.na(match(these_doors,doors)))

    if(!is_empty(correct)){
      
      # check that they've had a chance to learn about this door. otherwise, we don't trust that they know it's relevant.
      keep <- correct
      for (c in 1:length(correct)){
        if(first_feedback[correct[c]]>i){
          keep <- keep[-c]
        }
      }
      correct <- keep
      
      # confirm that there is still something in "correct", and carry on
      if(!is_empty(correct)){
        
        # record evidence for one or more strategies
        if(competitive){
          know_doors[length(correct),i] <- 1
        }else{
          know_doors[1:length(correct),i] <- 1
        }
        
      }

    }
  } 
  
  samples <- 1:nrow(events)
}


# -------------------------------------------------------------------------
# analyse by trial

if(method=="by_trial"){
  # find the point in each trial where they made their first mistake
  first_errors <- rep(0,1,max(unique(events$t)))
  for (i in unique(events$t)){
    tdata <- events %>% filter(t==i)
    try(if(tdata$door_cc[1]==0){first_errors[i] <- 1}else{first_errors[i] <- min(which(diff(tdata$door_cc)==-1))}, silent = TRUE)
  }
  
  # preallocate arrays
  trials <- unique(events$t)
  ntrials <- length(trials)
  nstrategies <- 4
  know_doors <- matrix(0,nstrategies,ntrials)
  # search through each trial for evidence that they know doors
  for (i in 1:ntrials){
    
    trial <- trials[i]
    tdata <- events %>% filter(t==trial)
    missing_evidence <- FALSE
    
    # if they haven't had a chance to experience a door as a target, treat it as context-irrelevant
    if((nses==1) &(trial < max(first_feedback_trial))){
      chance_doors <- doors[which(first_feedback_trial>=trial)]
      tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc))
    }
    
    # if the first door clicked is context-irrelevant, count that as evidence against them knowing any doors
    if(tdata$door_cc[1]==0){
      know_doors[,i] <- 0
      
    }else{
      # find out what doors they clicked
      # if we couldn't find their first mistake, they only clicked correct doors
      if(is.infinite(first_errors[trial])){
        these_doors <- unique(tdata$door)
        if(length(these_doors)<4){
          missing_evidence <- TRUE #they may have known more than the clicked doors; we can't evaluate that
        }
        
      # if we've found their first mistake, find the correct doors they found before the mistake
      }else{
        these_doors <- unique(tdata$door[1:first_errors[trial]])
      }
      
      # apply criteria for whether a door click counts as knowing that door
      # if we're requiring that they prioritise the same subset of doors before we consider clicks deliberate,
      if(specific_doors){
        door_idx <- match(these_doors,doors)
        know_doors[door_idx,i] <- 1
        
        # if we're allowing that people may click different subsets of doors, but must be consistent with the previous trial,
      }else{
        if(i>1){
          # find out what doors they selected on the previous trial
          those_doors <- events %>% filter(t==trials[i-1]) %>% pull(door) %>% unique()

          # if any correct doors were prioritised on both trials, count that as evidence of knowing however many doors 
          # have stayed the same
          if(any(!is.na(match(these_doors,those_doors)))){
            if(competitive){
              know_doors[length(which(!is.na(match(these_doors,those_doors)))),i] <- 1
            }else{
              know_doors[1:length(which(!is.na(match(these_doors,those_doors)))),i] <- 1
            }
            
            if(missing_evidence & !evaluate_all){
              know_doors[(length(which(!is.na(match(these_doors,those_doors))))+1):nstrategies,i] <- NA
            }
          }
        }
      }
      
    }
  }
  
  # if we're requiring that they continuing selecting e.g. door 11 as their k1 door, 
  if(specific_doors){
    # find the order in which they first selected the doors
    first_selection <- rep(0,nstrategies)
    for (i in 1:nstrategies){
      first_selection[i] <- min(which(know_doors[i,]==1))
    }
    # exclude trials on which previously-selected doors aren't among those selected
    for (i in 1:ntrials){
      for (j in 1:nstrategies){
        if(trials[i]>first_selection[j] & know_doors[j,i]==0){
          know_doors[,i] <- 0
        }
      }
    }  
    # sort by which doors were selected first
    know_doors <- know_doors[order(first_selection),]
  }
 
  samples <- trials 
}

# format
strategies <- data.frame(samples,k1=know_doors[1,],k2=know_doors[2,],k3=know_doors[3,],k4=know_doors[4,])
return(strategies)

}