apply_generate_nulls <- function(){

## K. Garner, 2025. Generate null distributions per participant
## take differences between observed and nulls and save outcome
###################################################################
rm(list=ls())
library(tidyverse)
exp_str = "ts"
###################################################################
# some functions to help
source("src-ent/ent_functions.R")

###################################################################
# load data
door_dat <- read.csv(paste("../doors-data/data-wrangled/exp", exp_str, 
                      "door_selections_for_ent.csv", sep="_"))
r_dat <- read.csv(file=paste("../doors-data/data-wrangled/exp", exp_str, 
                             "rscore-full.csv", sep="_"))

###################################################################
# define a function to generate a null distribution for one subject
get_null_per_sub <- function(door_dat, r_dat, subN, cntxN){
  tmp <- door_dat$door[door_dat$sub == subN & door_dat$context_assign_ent == cntxN]
  tmp_null <- generate_null_for_one_person(tmp)
  # the more chaotic someone is, the higher their null distribution will be
  tibble(sub = subN, context=cntxN, null = tmp_null, 
         r = r_dat$r[r_dat$sub == subN & r_dat$context == cntxN])
}

###################################################################
# now apply it across subjects
subNs <- rep(unique(door_dat$sub), times=length(unique(door_dat$context_assign_ent)))
cntxNs <- rep(unique(door_dat$context_assign_ent), each = length(unique(door_dat$sub)))
null_dists <- do.call(rbind, mapply(get_null_per_sub,
                                    subNs, cntxNs,
                                    MoreArgs = list(door_dat = door_dat,
                                                    r_dat = r_dat),
                                    SIMPLIFY = FALSE))

head(null_dists)
write.csv(null_dists, file=paste("../doors-data/data-wrangled/exp", exp_str, 
                                 "rnulls.csv", sep="_"),
          row.names=FALSE)
}