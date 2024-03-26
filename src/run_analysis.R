# lydia barnes, march 2024
# this script runs the analysis for data from the 'doors' project.

# TODO:
# fix accuracy and RT calculations

# if running for the first time:
#install.packages("renv")
#renv::init(project = '/Users/lydiabarnes/Documents/academe/projects/doors',bare = TRUE)

###
# sources
library(rstudioapi)
library(tidyverse)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(wd,'get_data.R'))

# essentials
project_path <- '/Users/lydiabarnes/Documents/academe/projects/doors'
data_path <- '/Users/lydiabarnes/Documents/academe/data/doors'

# settings
exp <- 'exp_ts' #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
subjects <- c('sub-01','sub-02')
ses <- 'ses-train' #session: 'ses-learn','ses-train','ses-test' 
mes <- 'clicks' #measure: 'clicks' or 'hovers'
if(mes=='clicks'){idx <- 1}else{idx <- 2}
version <- '20240325' #pilot: 20240325

###
# format the raw data 
grp_data <- data.frame(
  sub = integer(),
  test = integer(),
  t = integer(),
  cond = integer(),
  onset = numeric(),
  door = integer(),
  switch = integer(),
  door_c = integer(),
  offset = numeric()
)
for(sub in subjects){
  data <- get_data(data_path,exp,sub,ses)
  grp_data <- rbind(grp_data,data[[idx]])
}

###
# extract results: accuracy and RT (time to trial end)

#   by trial
res <- grp_data %>% group_by(sub,train,t,switch) %>% summarise(
  n_clicks = n_distinct(door),
  n_correct = sum(door_c),
  accuracy = n_correct/n_clicks,
  rt = max(offset)
) %>% select(!n_clicks:n_correct)
fnl <- file.path(project_path,'res',paste(version,'_trl.csv',sep = ""))
write_csv(res,fnl)

#   by subject
res <- grp_data %>% group_by(sub,train,switch) %>% summarise(
  n_clicks = n_distinct(door),
  n_correct = sum(door_c),
  accuracy = n_correct/n_clicks,
  rt = max(offset)
) %>% select(!n_clicks:n_correct)
fnl <- file.path(project_path,'res',paste(version,'_avg.csv',sep = ""))
write_csv(res,fnl) 

