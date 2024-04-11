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
version <- "20240409"
exp <- 'exp_ts' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
ses <- 'ses-train' #session: 'ses-learn','ses-train','ses-test'
mes <- 'clicks' #measure: 'clicks' or 'hovers'
title_sz <- 20
label_sz <- 20
mk_sz <- 2

###
# plot the data!
fnl <- file.path(project_path, "res", paste(paste(version,exp,mes,'avg',sep='_'),'.csv',sep = ""))
res <- read.csv(fnl)
res <- res %>% mutate(switch = case_when(switch==0~"Stay",switch==1~"Switch")) %>% mutate(train_type = as.character(train_type))
if(ses == 'ses-learn'){res <- res %>% filter(ses==1)}else if(ses == 'ses-train'){res <- res %>% filter(ses==2)}else if(ses=='ses-test'){res <- res %>% filter(ses==3)}

if(ses == 'ses-learn'){ #don't split learning phase data by training type, as that's irrelevant
  res <- res %>% mutate(context = case_when(context==1~"Context A",context==2~"Context B")) 
  res <- res %>% filter(switch != 'Switch') #drop that one switch trial during the learn phase, as it could skew results
  
  res %>% 
    ggplot() + 
    geom_hline(yintercept=0.25,linetype='solid',linewidth=1,alpha=1,color='black') +
    geom_jitter(aes(x=context,y=accuracy),alpha = .5,size = mk_sz) +
    stat_summary(aes(x=context,y=accuracy),fun.data = "mean_cl_normal",geom = "pointrange",linewidth = 2,size = mk_sz/2,alpha = 1) +
    stat_summary(aes(x=context,y=accuracy),fun = "mean",geom = "line",linewidth = 1,alpha = 1) +
    theme_minimal() + 
    scale_x_discrete(labels = c("Context A","Context B")) +
    labs(title = "", x = "Context", y = "Accuracy (%)") +
    theme(plot.title = element_text(size = title_sz),
          axis.text.x = element_text(size = label_sz),axis.text.y = element_text(size = label_sz),legend.text = element_text(size=label_sz),
          axis.title.x = element_text(size = label_sz),axis.title.y = element_text(size = label_sz),legend.title=element_text(size=label_sz))
  
}else{
  res %>% 
    ggplot() + 
    geom_hline(yintercept=0.25,linetype='solid',linewidth=1,alpha=1,color='black') +
    geom_jitter(aes(x=train_type,y=accuracy,shape=switch,color=switch),position = position_jitterdodge(dodge.width = .3,jitter.width=.1),alpha = .5,size = mk_sz) +
    stat_summary(aes(x=train_type,y=accuracy,shape=switch,color=switch),fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .3),linewidth = 2,size = mk_sz/2,alpha = 1) +
    stat_summary(aes(x=train_type,y=accuracy,color=switch),fun = "mean",geom = "line", position = position_dodge(width = .3),linewidth = 1,alpha = 1) +
    theme_minimal() + 
    scale_shape_discrete(name = "Switch Condition", labels = c("Stay","Switch")) +
    scale_color_lancet(guide="none") +
    scale_x_discrete(labels = c("Low Switch","High Switch")) +
    labs(title = "", x = "Training Condition", y = "Accuracy (%)") +
    theme(plot.title = element_text(size = title_sz),
          axis.text.x = element_text(size = label_sz),axis.text.y = element_text(size = label_sz),legend.text = element_text(size=label_sz),
          axis.title.x = element_text(size = label_sz),axis.title.y = element_text(size = label_sz),legend.title=element_text(size=label_sz))
}
fnl <- file.path(project_path,'fig',paste(paste(version,exp,ses,mes,'avg',sep='_'),'.pdf',sep = ""))
ggsave(fnl,plot = last_plot())
