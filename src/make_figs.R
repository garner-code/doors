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
exp <- 'exp_lt' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
ses <- 'ses-test' #session: 'ses-learn','ses-train','ses-test'
mes <- 'clicks' #measure: 'clicks' or 'hovers'
title_sz <- 20
label_sz <- 20
mk_sz <- 2

###
# plot the data!
fnl <- file.path(project_path, "res", paste(paste(version,exp,mes,'avg',sep='_'),'.csv',sep = ""))
res <- read.csv(fnl)
res <- res %>% mutate(switch = case_when(switch==0~"Switch",switch==1~"Stay")) %>% mutate(train_type = as.character(train_type))
if(ses=='ses-test'){res <- res %>% filter(ses==3)}else{res <- res %>% filter(ses==2)}

res %>% 
  ggplot() + 
  geom_hline(yintercept=0.5,linetype='solid',linewidth=1,alpha=1,color='black') +
  geom_jitter(aes(x=train_type,y=accuracy,shape=switch,color=switch),position = position_jitterdodge(dodge.width = .3,jitter.width=.1),alpha = .5,size = mk_sz) +
  stat_summary(aes(x=train_type,y=accuracy,shape=switch,color=switch),fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .3),linewidth = 2,size = mk_sz/2,alpha = 1) +
  stat_summary(aes(x=train_type,y=accuracy,color=switch),fun = "mean",geom = "line", position = position_dodge(width = .3),linewidth = 1,alpha = 1) +
  theme_minimal() + 
  scale_shape_discrete(name = "Switch Condition", labels = c("Switch","Stay")) +
  scale_color_lancet(guide="none") +
  scale_x_discrete(labels = c("Low Switch","High Switch")) +
  labs(title = "", x = "Training Condition", y = "Accuracy (%)") +
  theme(plot.title = element_text(size = title_sz),
        axis.text.x = element_text(size = label_sz),axis.text.y = element_text(size = label_sz),legend.text = element_text(size=label_sz),
        axis.title.x = element_text(size = label_sz),axis.title.y = element_text(size = label_sz),legend.title=element_text(size=label_sz))

fnl <- file.path(project_path,'fig',paste(paste(version,exp,ses,mes,'avg',sep='_'),'.pdf',sep = ""))
ggsave(fnl,plot = last_plot())
