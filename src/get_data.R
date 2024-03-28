get_data <- function(path,exp,sub,ses){
  # reads in trial info and sample data from 'trls' and 'beh' files and formats into a one-row-per-trial data frame

  trials_here <- file.exists(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_trls.tsv',sep='_')))
  resps_here <- file.exists(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_beh.tsv',sep='_')))

  if(trials_here & resps_here){
    trials <- read.table(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_trls.tsv',sep='_')),header = TRUE)
    resps <- read.table(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_beh.tsv',sep='_')),header = TRUE)
    
    ###
    # extract the start and end of each trial
    resps <- resps %>%  
      mutate(door_correct = case_when(door_p>0~1,door_p==0~0,.default=0)) %>% 
      mutate(change = c(0,diff(open_d))) %>% 
      mutate(change_d = c(0,diff(door))) %>%
      mutate(on = case_when(change==9~TRUE,change==1~TRUE,change==-9~FALSE,change==-1~FALSE,change==-8~TRUE,change==8~TRUE,change_d!=0~TRUE,.default=FALSE)) %>% 
      mutate(off = case_when(change==-9~TRUE,change==-1~TRUE,change==9~FALSE,change==1~FALSE,change==-8~TRUE,change==8~TRUE,change_d!=0~TRUE,.default=FALSE)) %>% 
      mutate(off = c(off[2:length(off)],TRUE))
    offset <- resps %>% filter(off == TRUE) %>% select(onset) %>% rename(offset = onset)
    onset <- resps %>% filter(on == TRUE) %>% select(!(change:off)) 
    if(length(offset$offset) > length(onset$onset)){
      resps$on[1]=TRUE
      onset <- resps %>% filter(on == TRUE) %>% select(!(change:off)) 
    }
    resps <- bind_cols(onset,offset) %>% select(!door_p:y)
    
    # rename 'train' and 'test' to 'group' so treatment label is consistent across sessions
    if(ses == 'ses-train'){
      resps <- resps %>% rename(group = train)
    }else{
      resps <- resps %>% rename(group = test)
    }
    
    # separate click and hover responses
    clicks <- resps %>% filter(open_d == 1) %>% select(!open_d) 
    tmp <- resps %>% filter(open_d == 9) %>% select(!open_d) 
    hovers <- tmp[0,]
    for(i in 1:2){
      doors <- resps %>% filter(cond == i) %>% filter(door_correct == 1) %>% group_by(cond) %>% distinct(door) %>% pivot_wider(names_from = cond, values_from = door)
      tmp2 <- tmp %>% mutate(door_correct = case_when(door %in% doors[[1]][[1]]~1,.default=0) )
      hovers <- rbind(hovers,tmp2)
    }

    # if these are task-switching data, 'cond' should reflect switch/stay, not context 1/2
    if(exp=='exp_ts'){
      clicks <- clicks %>% mutate(cond = c(0,case_when(diff(cond) !=0 ~ 1,.default=0)))
      hovers <- hovers %>% mutate(cond = c(0,case_when(diff(cond) !=0 ~ 1,.default=0)))
    }
    
    return(list(clicks,hovers))
    
  }else{
    warning(paste('check data for ',exp,sub,ses,sep='/'))
  }
}