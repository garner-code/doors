# lydia barnes, march 2024
# this script extracts, formats, and summarises data from the 'doors' project.

# TODO:
# remove redundancy bw door_correct and door_cc
# save full click and hover data as well as trial- and subject-grouped results
# produce alternate results grouped by door identity instead of nclicks

###
# sources
library(rstudioapi)
library(tidyverse)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(wd,'get_data.R'))
source(file.path(wd,'get_subs.R'))

# essentials
getwd() #check what directory (folder) we are in. this will be the doors project analysis directory, provided you have opened the project by clicking on
project_path <- getwd() #if you open the project thru doors.Rproj, your working directory will automatically be the project path

# settings 
#   !you will want to update these settings a lot during piloting, as the task code or the way you test changes, and as you test participants on different subsets of the task phases
version <- 'pilot-data-01' #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- 'exp_ts' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
sess <- c('ses-learn','ses-train') #session: 'ses-learn','ses-train','ses-test'. can select one (e.g. ses <- c('ses-learn')) or multiple (e.g. ses <- c('ses-train','ses-test'))

#   !you will need to change this to match the location of OneDrive on your personal computer
data_path <- file.path('/Users/lydiabarnes/OneDrive - UNSW/task switch and transfer/data-sandpit',version)
if (!dir.exists(data_path)){
  stop(paste0(data_path,' does not exist'))
}

#   !you can change the following settings if you want to, but the defaults will usually be fine
mes <- 'clicks' #measure: 'clicks' or 'hovers'. usually want 'clicks'.
if(mes=='clicks'){idx <- 1}else{idx <- 2}
apply_threshold <- FALSE #only retain events that lasted more than a given duration?
min_dur <- 0.1 #minimum duration 

subs <- get_subs(exp,version) #now that we know which experiment and version we want to look at, let's load the list of subjects from get_subs

###
# format the raw data 
grp_data <- data.frame(
  sub = integer(),
  ses = integer(),
  t = integer(),
  context = integer(),
  door = integer(),
  door_correct = integer(),
  on = numeric(),
  off = numeric(),
  door_cc = integer(),
  door_oc = integer(),
  switch = integer(),
  train_type = integer()
)
for(sub in subs){
  for(ses in sess){
    
    train_type <- NA
    if(ses == 'ses-test'){
      train_type <- grp_data %>% filter(sub == sub & ses == 2) %>% select(train_type)
    }
    data <- get_data(data_path,exp,sub,ses,train_type,apply_threshold,min_dur)
    grp_data <- rbind(grp_data,data[[idx]]) 
  }
}

###
# extract results: accuracy and RT (time to trial end)
#   by trial
res <- grp_data %>% group_by(sub,ses,t,context,train_type) %>% summarise(
  switch = max(switch),
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

