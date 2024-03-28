# lydia barnes, march 2024
# generates figues from 'doors' project RT and accuracy outputs

###
# sources
library(tidyverse)
library(ggforce)
library(ggsci)

# essentials
project_path <- getwd()

# settings
version <- "20240325"
exp <- 'exp_ts' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
ses <- 'ses-train' #session: 'ses-learn','ses-train','ses-test'
mes <- 'clicks' #measure: 'clicks' or 'hovers'

title_sz <- 20
label_sz <- 20
mk_sz <- 2

###
# plot the data!
fnl <- file.path(project_path, "res", paste(paste(version,exp,ses,mes,'avg',sep='_'),'.csv',sep = ""))
results <- read.csv(fnl)
results <- results %>% mutate(cond = case_when(cond==0~"Low Switch",cond==1~"High Switch")) %>% mutate(group = as.character(group))

results %>% 
  ggplot() + 
  geom_hline(yintercept=0.5,linetype='solid',linewidth=1,alpha=1,color='black') +
  geom_jitter(aes(x=group,y=accuracy,shape=cond,color=cond),position = position_jitterdodge(dodge.width = .3,jitter.width=.1),alpha = .5,size = mk_sz) +
  stat_summary(aes(x=group,y=accuracy,shape=cond,color=cond),fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .3),linewidth = 2,size = mk_sz/2,alpha = 1) +
  stat_summary(aes(x=group,y=accuracy,color=cond),fun = "mean",geom = "line", position = position_dodge(width = .3),linewidth = 1,alpha = 1) +
  theme_minimal() + 
  scale_shape_discrete(name = "Switch Condition", labels = c("Switch","Stay")) +
  scale_color_lancet(guide="none") +
  scale_x_discrete(labels = c("Low Switch","High Switch")) +
  labs(title = "", x = "Training Condition", y = "Accuracy (%)") +
  theme(plot.title = element_text(size = title_sz),
        axis.text.x = element_text(size = label_sz),axis.text.y = element_text(size = label_sz),legend.text = element_text(size=label_sz),
        axis.title.x = element_text(size = label_sz),axis.title.y = element_text(size = label_sz),legend.title=element_text(size=label_sz))

