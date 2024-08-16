# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.

### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%

source(file.path(getwd(), "src", "get_subs.R"))
source(file.path(getwd(), "src", "get_switch.R"))
source(file.path(getwd(), "src", "get_data.R"))
source(file.path(getwd(), "src", "get_context_changes.R"))
source(file.path("src","get_transition_probabilities.R"))
source(file.path("src","get_learned_doors.R"))

### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
version <- "study-01" # pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
sess <- c("ses-learn","ses-train","ses-test") # session: 'ses-learn','ses-train','ses-test'. can select one (e.g. ses <- c('ses-learn')) or multiple (e.g. ses <- c('ses-train','ses-test'))

### paths

# !if you open the project thru doors.Rproj, your working directory will automatically be the
# project path
project_path <- getwd()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}

# !you will need to change the data path to match the location of OneDrive on your personal
# computer
data_path <- file.path("/Users/lydiabarnes/OneDrive - UNSW/task switch and transfer/data-sandpit", version)
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
subs <- get_subs(exp, version)

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), on = numeric(), off = numeric(), subses = integer(), door_oc = integer(), 
  switch = integer(), train_type = integer(), transfer = integer(), full_transfer_first = integer(),
  original_house = integer()
)

# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) {
    train_type <- NA
    context_one_doors <- NA
    if (ses == "ses-test") {
      train_type <- grp_data %>%
        filter(sub == sid, ses == 2) %>%
        select(train_type) %>% 
        unique() %>% 
        pull()
      train_doors <- grp_data %>% 
        filter(sub==sid,ses==ses,door_cc==1) %>% 
        select(door,context) %>% 
        unique()
    }
    data <- get_data(data_path, exp, sub, ses, train_type, train_doors) # load and format raw data
    grp_data <- rbind(grp_data, data) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
  }
}

# track whether context-incorrect clicks in the test phase land on doors that were learned in the train phase
if(exp=="exp_lt"){
  door_lc <- get_learned_doors(grp_data)
  grp_data$door_lc <- door_lc
}else{
  grp_data <- grp_data %>% mutate(door_lc = c(kronecker(matrix(1, nrow(res), 1), NA)))
}

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract accuracy and response time averages from event data

# by trial
res <- grp_data %>%
  group_by(sub, ses, t, context, train_type, transfer, full_transfer_first, original_house) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc), n_lc = sum(door_lc),
    accuracy = n_cc / n_clicks,
    general_errors = (n_clicks - n_cc - n_oc) / n_clicks,
    setting_errors = n_oc / n_clicks,
    learned_setting_errors = n_lc / n_clicks
  )
rt <- grp_data %>%
  group_by(sub, ses, t, context, train_type, transfer) %>%
  filter(door_cc == 1) %>%
  summarise(rt = min(off)) # time to first correct click offset
res$rt <- rt$rt

# add accuracy calculated the way it was for points during the task
win <- 4-res$n_clicks >= 0
res$win <- win

# context changes
#   record the number of times they switch between door_cc, door_oc, and door_nc on each trial. 
#   if it's a non-switch trial, assume that they're coming from door_cc. if it's a switch, assume that they're coming from door_oc.
grp_data <- grp_data %>% mutate(door_nc = case_when(door_cc==1 ~ 0, door_oc == 1 ~ 0, .default=1))
c(res$context_changes, res$learned_context_changes) %<-% get_context_changes(grp_data)

fnl <- file.path(project_path, "res", paste(paste(exp, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

# by subject
res <- res %>%
  group_by(sub, ses, context, switch, train_type, transfer, full_transfer_first, original_house) %>%
  summarise_all(mean)

# transition probabilities
#   record how likely they are to move between the partial-transfer doors pairs during the train phase
res <- res %>% ungroup() %>% mutate(transition_probabilities = c(kronecker(matrix(1, nrow(res), 1), NA)))
if(exp=="exp_lt"){
  res$transition_probabilities[which(res$ses==2)] <- get_transition_probabilities(grp_data)
}

fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)
