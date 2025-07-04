# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.

### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%
library(stringr)

source(file.path("src", "get_subs.R"))
source(file.path("src", "get_switch.R"))
source(file.path("src", "get_data.R"))
source(file.path("src","get_setting_stability.R"))
source(file.path("src","get_transition_probabilities.R"))
source(file.path("src","get_learned_doors.R"))

### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
version <- "data_analysis_touchy" 
exp <- "flexibility"
sess <- c("ses-learn","ses-train","ses-test")

# if (exp == "multitasking") {
#   sess <- c('ses-mts', "ses-learn","ses-train","ses-test")
# } else {
#   sess <- c("ses-learn","ses-train","ses-test")
# }

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
file_path <- "~/Downloads"
exp_path <- str_glue("/{exp}/{version}")
data_path <- file.path(file_path + exp_path)

if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
files <- list.files(data_path, pattern = str_glue('.*(beh.tsv)'), recursive = T) 
subs <- unique(str_split_i(files, "/", 1))

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), subses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), door_oc = integer(), on = numeric(), off = numeric(), 
  switch = integer(), train_type = integer())

# create empty data frame for the response time data
grp_ons <- data.frame(
  sub = integer(), ses = integer(), t = integer(), context = integer(), on = integer()
)

# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) { 
    train_type <- NA
    context_one_doors <- NA
    train_doors <- NA # ???
  
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
      grp_data <- rbind(grp_data, data$resps) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
      grp_ons <- rbind(grp_ons, data$ons) # add event onset times to data frame
    }
  }

# track whether context-incorrect clicks in the test phase land on doors that were learned in the train phase
# if(exp=="exp_lt"){
#   door_lc <- get_learned_doors(grp_data)
#   grp_data <- grp_data %>% add_column(door_lc = door_lc, .after="door_oc")
# }else{
#   grp_data <- grp_data %>% mutate(door_lc = c(kronecker(matrix(1, nrow(grp_data), 1), NA)), .after="door_oc")
# }
grp_data <- grp_data %>% mutate(door_lc = c(kronecker(matrix(1, nrow(grp_data), 1), NA)), .after="door_oc")
grp_data <- get_setting_stability(grp_data) # track when they changed context into the correct or other context's door set - KG: this line may not be necessary
grp_data <- grp_data %>% mutate(door_nc = case_when(door_cc==1 ~ 0, door_oc == 1 ~ 0, .default=1), .after="door_oc")

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract accuracy and response time averages from event data

# by trial
res <- grp_data %>%
  group_by(sub, ses, subses, t, context, train_type) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc), n_lc = sum(door_lc), n_nc = sum(door_nc),
    setting_sticks = select_oc[1],
    setting_slips = max(select_oc_late),
    context_changes = sum(select_cc)+sum(select_oc),
    accuracy = n_cc / n_clicks,
    setting_errors = n_oc / n_clicks,
    general_errors = n_nc / n_clicks,
    learned_setting_errors = n_lc / n_clicks
)

# re-label exp_lt test phase "switch" trials as stay trials
# if (exp == "exp_lt"){
#   res <- res %>% 
#     mutate(switch = case_when(switch==1 & ses==3 ~ 0, .default = switch))
# }

# calculate context change rates
res$context_changes[intersect(which(res$switch==1),which(res$ses==2))] <- res$context_changes[intersect(which(res$switch==1),which(res$ses==2))]-1
rt <- grp_data %>%
  group_by(sub, ses, subses, t, context, train_type) %>%
  filter(door_cc == 1) %>%
  summarise(rt = min(off)) # time to first correct click offset - KG: do we want onset instead? Also, we now need to get the original ontime recorded for each session, and subtract the value from here
rt <- inner_join(grp_ons, rt, by=c('sub', 'ses', 't', 'context'))
res$rt <- rt$rt - rt$on
res$win <- 4-res$n_clicks >= 0

# make sure we can select just the first-click-correct trials to calculate avg rt over trials
res$first_click_correct <- grp_data %>% 
  mutate(diff_cc = diff(c(1,door_cc))) %>% 
  mutate(diff_t = diff(c(0,t))) %>% 
  mutate(first_click_correct = case_when(diff_cc == 0 & diff_t == 1 ~ 1, .default = 0)) %>% 
  group_by(sub, ses, subses, t, context, train_type) %>%
  summarise(first_click_correct = first_click_correct[1]) %>% 
  pull(first_click_correct)
res <- res %>% 
  mutate(rt_correct = case_when(first_click_correct == 1 ~ rt))

# trim RTs
if (exp=="flexibility"){
  res <- res %>% filter(rt<=10) %>% ungroup() %>% group_by(ses,context,switch) %>% filter(rt<=(mean(rt)+(3*sd(rt)))) # so RT trimming happened at the group level. Need to change this and apply to both experiments
}

fnl <- file.path(project_path, "res", paste(paste(exp, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

# by subject
#   grouping by subsession
res_ss <- res %>%
  ungroup() %>% 
  group_by(sub, ses, subses, context, switch, train_type) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE) )) %>% 
  select(!first_click_correct)
res_ss <- res_ss %>% ungroup() %>% mutate(transition_probabilities = c(kronecker(matrix(1, nrow(res_ss), 1), NA)))
# if(exp=="exp_lt"){
#   res_ss$transition_probabilities[which(res_ss$ses==2)] <- get_transition_probabilities(grp_data)
# }
res_ss <- res_ss %>% select(!t)
fnl <- file.path(project_path, "res", paste(paste(exp, "avg-ss", sep = "_"), ".csv", sep = ""))
write_csv(res_ss, fnl)

#   just grouping by session
res <- res %>%
  group_by(sub, ses, context, switch, train_type) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE) )) %>% 
  select(!first_click_correct)
res <- res %>% ungroup() %>% mutate(transition_probabilities = c(kronecker(matrix(1, nrow(res), 1), NA)))
# if(exp=="exp_lt"){
#   res$transition_probabilities[which(res$ses==2)] <- get_transition_probabilities(grp_data)
# }
res <- res %>% select(!subses)
fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)
