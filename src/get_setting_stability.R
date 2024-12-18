# lydia barnes, august 2024
# if someone is deep into a set of doors, they may struggle to respond to a cue to change contexts.
# this could be independent from, or anti-correlated with, how likely they are to make a setting error later in a trial
# both of these things are bundled into setting_errors. this function separates the two.

get_setting_stability <- function(data){
  
  data <- data %>% 
    group_by(sub,ses,t) %>% 
    mutate(
      scca = case_when(diff(c(1,door_cc))>0~1,.default=0), 
      sccb = case_when(diff(c(0,door_cc))>0~1,.default=0), 
      s_cc = case_when(ses==2 & switch==1 ~ sccb, .default=scca),
      soca = case_when(diff(c(0,door_oc))>0~1,.default=0), 
      socb = case_when(diff(c(1,door_oc))>0~1,.default=0), 
      s_oc = case_when(ses==2 & switch==1 ~ socb, .default=soca),
      s_oc_late = case_when(diff(c(0,t))~0,.default=soc)
    ) %>% 
    select(!c(on,off,train_type:original_house))
  
  select_context <- data.frame(s_cc=integer(),s_oc=integer(),s_oc_late=integer(),s_total=integer(),s_cumulative=integer())
  for(su in unique(data$sub)){
    for(se in unique(data$ses)){
      
      sdata <- data %>% filter(sub==su,ses==se)
      for (trial in unique(sdata$t)){
        
        tdata <- sdata %>% filter(t==trial)
        door_cc <- tdata %>% pull(door_cc) #when do they click in the current context?
        door_oc <- tdata %>% pull(door_oc) #when do they click in the other context?
        
        if(unique(tdata$ses) == 2 && unique(tdata$switch) == 1){
          door_oc <- c(1,door_oc) #assume that, on uncued switch trials, they start in the other context
          door_cc <- c(0,door_cc)
        }else{
          door_cc <- c(1,door_cc) #otherwise, assume that they start in the correct context
          door_oc <- c(0,door_oc)
        }
        
        #get context changes
        s_cc <- diff(door_cc)
        s_oc <- diff(door_oc) 
        
        #ignore switches away, and only count switches into
        s_cc[s_cc<0] <- 0
        s_oc[s_oc<0] <- 0 
        
        #make a variable that ignores the first switch
        s_oc_late <- s_oc
        s_oc_late[1] <- 0
        
        tmp <- data.frame(s_cc,s_oc,s_oc_late) %>% mutate(s_total = case_when(s_cc==1~1,s_oc==1~1,.default=0)) %>% mutate(s_cumulative = cumsum(s_total))
        select_context <- rbind(select_context,tmp)

      } 
    }
  }

  return(select_context)
}