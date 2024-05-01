# lydia barnes, march 2024
# this script extracts, formats, and summarises data from the 'doors' project.

# TODO:
# remove redundancy bw door_correct and door_cc
# produce alternate results grouped by door identity instead of nclicks

###
# sources
library(tidyverse)
source(file.path(getwd(),'src','get_data.R'))
source(file.path(getwd(),'src','get_subs.R'))

###
# settings 

#   !you will want to update these settings a lot during piloting, when the task code or the way you test changes, or when you test participants on different subsets of the task phases
version <- 'study-01' #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- 'exp_lt' #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
sess <- c('ses-learn','ses-train','ses-test') #session: 'ses-learn','ses-train','ses-test'. can select one (e.g. ses <- c('ses-learn')) or multiple (e.g. ses <- c('ses-train','ses-test'))

#   !you can change the following settings if you want to, but the defaults will usually be fine
mes <- 'clicks' #measure: 'clicks' or 'hovers'. usually want 'clicks'.
if(mes=='clicks'){idx <- 1}else{idx <- 2}
apply_threshold <- FALSE #only retain events that lasted more than a given duration?
min_dur <- 0.1 #minimum duration

###
# paths

#   !if you open the project thru doors.Rproj, your working directory will automatically be the project path
project_path <- getwd() 
if(!dir.exists(file.path(project_path,'res'))){ # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path,'res'))
}

#   !you will need to change the data path to match the location of OneDrive on your personal computer
data_path <- file.path('/Users/lydiabarnes/OneDrive - UNSW/task switch and transfer/data-sandpit',version) 
if (!dir.exists(data_path)){
  stop(paste0(data_path,' does not exist'))
}

###
# load an up-to-date list of participants
subs <- get_subs(exp,version) 

###
# extract events from the raw data

#   make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), #subject number
  ses = integer(), #session (learn, train, or test)
  t = integer(), #trial number
  context = integer(), 
  door = integer(), #which door they are clicking or hovering over
  door_correct = integer(), #whether the door is the target
  on = numeric(), #when they began this click/hover
  off = numeric(), #when they released this click/hover (useful if we want to only count hovers that last more than some duration)
  door_cc = integer(), #whether the door belongs to the current context
  door_oc = integer(), #whether the door belongs to the other context (for calculating setting accuracy)
  switch = integer(), #whether the context on this event is different to the previous event's context
  train_type = integer() #whether this person's train phase had a high or low switch rate (only relevant for train and test)
)

#   for each subject and session, use the function 'get_data' to load their raw data and attach it to our 'grp_data' data frame with one measurement (row) per event (click or hover)
for(sub in subs){
  for(ses in sess){
    train_type <- NA
    if(ses == 'ses-test'){ #we calculate people's training group from the switch rate in their train phase data. copy that information into the test phase data
      train_type <- grp_data %>% filter(sub == sub & ses == 2) %>% select(train_type)
    }
    data <- get_data(data_path,exp,sub,ses,train_type,apply_threshold,min_dur) #load and format raw data
    grp_data <- rbind(grp_data,data[[idx]]) #add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
  }
}

#   save the formatted data
fnl <- file.path(project_path,'res',paste(paste(version,exp,mes,'evt',sep='_'),'.csv',sep = ""))
write_csv(grp_data,fnl)

###
# extract accuracy and response time averages from event data

#   by trial
res <- grp_data %>% group_by(sub,ses,t,context,train_type) %>% summarise(
  switch = max(switch), #if the context switched for any event on this trial, it's a switch trial 
  n_clicks = n(),
  n_correct = sum(door_correct), 
  n_cc = sum(door_cc),
  n_oc = sum(door_oc),
  accuracy = n_correct/n_clicks,
)
rt <- grp_data %>% group_by(sub,ses,t,context,train_type) %>% filter(door_correct==1) %>% summarise(rt = min(off)) #time to first correct click offset
res$rt <- rt$rt
fnl <- file.path(project_path,'res',paste(paste(version,exp,mes,'trl',sep='_'),'.csv',sep = ""))
write_csv(res,fnl)

#   by subject
res <- res %>% group_by(sub,ses,context,switch,train_type) %>% summarise(
  n_clicks = mean(n_clicks),
  n_cc = mean(n_cc),
  n_oc = mean(n_oc),
  rt = mean(rt),
  accuracy = mean(accuracy)
) 
fnl <- file.path(project_path,'res',paste(paste(version,exp,mes,'avg',sep='_'),'.csv',sep = ""))
write_csv(res,fnl) 

