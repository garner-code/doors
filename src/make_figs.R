# lydia barnes, march 2024
# generates figues from 'doors' project RT and accuracy outputs

###
# sources
library(tidyverse)
library(ggforce)
library(ggsci)

# essentials
project_path <- '/Users/lydiabarnes/Documents/academe/projects/doors'

# settings
version <- "20240325"
title_sz <- 20
label_sz <- 20
mk_sz <- 2

###
# plot the data!
fnl <- file.path(project_path, "res", paste(version,'_avg.csv',sep=""))
results <- read.csv(fnl)
results <- results %>% mutate(switch = case_when(switch==0~"Low Switch",switch==1~"High Switch")) %>% mutate(train = as.character(train))

results %>% 
  ggplot() + 
  geom_hline(yintercept=50,linetype='solid',linewidth=1,alpha=1,color='black') +
  geom_jitter(aes(x=train,y=accuracy,shape=switch,color=switch),position = position_jitterdodge(dodge.width = .3,jitter.width=.1),alpha = .5,size = mk_sz) +
  stat_summary(aes(x=train,y=accuracy,shape=switch,color=switch),fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .3),linewidth = 2,size = mk_sz/2,alpha = 1) +
  #stat_summary(aes(x=train,y=accuracy,color=switch),fun = "mean",geom = "line", position = position_dodge(width = .3),linewidth = 1,alpha = 1) +
  theme_minimal() + 
  scale_shape_discrete(name = "Switch Condition", labels = c("Switch","Stay")) +
  scale_color_lancet(guide="none") +
  scale_x_discrete(labels = c("Low Switch","High Switch")) +
  labs(title = "", x = "Training Condition", y = "Accuracy (%)") +
  theme(plot.title = element_text(size = title_sz),
        axis.text.x = element_text(size = label_sz),axis.text.y = element_text(size = label_sz),legend.text = element_text(size=label_sz),
        axis.title.x = element_text(size = label_sz),axis.title.y = element_text(size = label_sz),legend.title=element_text(size=label_sz))

