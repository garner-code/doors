get_data <- function(path,exp,sub,ses,train_type){
  # reads in trial info and sample data from 'trls' and 'beh' files and formats into a one-row-per-trial data frame

  trials_here <- file.exists(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_trls.tsv',sep='_')))
  resps_here <- file.exists(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_beh.tsv',sep='_')))

  if(trials_here & resps_here){
    trials <- read.table(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_trls.tsv',sep='_')),header = TRUE)
    resps <- read.table(file.path(path,exp,sub,ses,'beh',paste(sub,ses,'task-mforage_beh.tsv',sep='_')),header = TRUE)
    
    ###
    # extract the start and end of each trial
    resps <- resps %>%  
      rename(context = cond) %>% 
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
    if(ses == 'ses-train'){resps <- resps %>% rename(ses = train)}else{resps <- resps %>% rename(ses = test)}
    
    # separate click and hover responses
    clicks <- resps %>% filter(open_d == 1) %>% select(!open_d) 
    tmp <- resps %>% filter(open_d == 9) %>% select(!open_d) 
    hovers <- tmp[0,]
    for(i in 1:2){
      doors <- resps %>% filter(context == i) %>% filter(door_correct == 1) %>% group_by(context) %>% distinct(door) %>% pivot_wider(names_from = context, values_from = door)
      tmp2 <- tmp %>% mutate(door_correct = case_when(door %in% doors[[1]][[1]]~1,.default=0) )
      hovers <- rbind(hovers,tmp2)
    }

    # add switch/stay variable
    clicks <- clicks %>% mutate(switch = c(0,case_when(diff(context) !=0 ~ 1,.default=0)))
    hovers <- hovers %>% mutate(switch = c(0,case_when(diff(context) !=0 ~ 1,.default=0)))
    
    if(ses == 'ses-train'){
      # calculate the switch rate
      
      sr <- clicks %>% summarise(sr = mean(switch))
      if(sr$sr[[1]]<.05){ #low switch rate
        clicks <- clicks %>% mutate(train_type = c(kronecker(matrix(1,nrow(clicks),1),1)))
        hovers <- hovers %>% mutate(train_type = c(kronecker(matrix(1,nrow(hovers),1),1)))
      }else{
        clicks <- clicks %>% mutate(train_type = c(kronecker(matrix(1,nrow(clicks),1),2)))
        hovers <- hovers %>% mutate(train_type = c(kronecker(matrix(1,nrow(hovers),1),2)))
      }
    }else{
      # use the switch rate we calculated from their training data
      
      clicks <- clicks %>% mutate(train_type = c(kronecker(matrix(1,nrow(clicks),1),train_type$train_type[[1]])))
      hovers <- hovers %>% mutate(train_type = c(kronecker(matrix(1,nrow(hovers),1),train_type$train_type[[1]])))
    }
    
    return(list(clicks,hovers))
    
  }else{
    warning(paste('check data for ',exp,sub,ses,sep='/'))
  }
}