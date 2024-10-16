# lydia barnes, september 2024

# reads and formats click data 
# converts click data to count data for each of six possible paths
# generates dirichlet distribution from path count data
# extracts parameters and estimates entropy (our routineness measure)

# -------------------------------------------------------------------------
# load resources and set parameters
rm(list=ls())
library(tidyverse)
source(file.path("src-dirich", "count_paths.R"))
source(file.path("src-dirich", "make_dirichlet.R"))
project_path <- getwd()
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
start_point = "favourite" # where to start counting each routine. "fixed" (top-left for everyone) or "favourite" (each person's preferred trial start point)

# -------------------------------------------------------------------------
# load data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
data <- read.csv(fnl)

# -------------------------------------------------------------------------
# loop through participants and contexts
routineness <- data.frame()
for (sid in unique(data$sub)){
  print(sprintf('subject %d', sid))
  
  for(ss in unique(data$ses)){
    for (ctx in unique(data$context)){
      clicks <- data %>% filter(sub==sid,ses==ss,context==ctx,door_cc==1,switch==0) # get context-correct stay trial clicks only
      counts <- count_paths(clicks, start_point)
      tmp <- make_dirichlet(counts)
      routineness <- rbind(routineness,cbind(sub=sid,ses=ss,context=ctx,train_type=unique(clicks$train_type),
                                             nclicks_avail=nrow(clicks),nclicks_used=sum(counts$counts)*4,tmp))
    }
    
  }
}

# -------------------------------------------------------------------------
# check how much data we were able to use
#   we only use sets of four unique clicks for the modelling above, i.e. [1 2 1 2 3 4] counts as [1 2 . . 3 4]
print(sprintf('avg. missed clicks = %.2f', routineness %>% mutate(missed = nclicks_avail - nclicks_used) %>% pull(missed) %>% mean))

# -------------------------------------------------------------------------
# save the data
fnl <- file.path(project_path, "res", paste(paste(exp, "routines", sep = "_"), ".csv", sep = ""))
write_csv(routineness, fnl)
