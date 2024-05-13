# lydia barnes, april 2024

# this script plots optimal and observed search path data for the doors project

# NB: at 2024.04.30, i've calculated optimal paths for the contexts used in the learn and train
# phases.  for exp_ts, those same contexts appear in the test phase, too.  i haven't calculated
# optimal paths for the new contexts introduced during exp_lt's test phase. we plan to use people's
# path adherence during the train phase to predict performance gains during training and test, so
# at the moment, test phase optimal paths aren't relevant. if they become important, extend the
# context indices in format_paths.R and in get_shortest_path.py.

# TODO:

### sources
library(tidyverse)
library(tidyjson)

### settings

# which dataset?
version <- "study-01"  #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt"  #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
mes <- "clicks"  #measure: 'clicks' or 'hovers'. usually want 'clicks'.

# which subject, algorithm, context, session, and trials would you like to view?
subject <- 6  #subject id, as integer
alg <- "hc"  #shortest path algorithm: 'hc' (hamiltonian cycle) or 'tsp' (travelling salesperson)
ctx <- 2  #context: 1 or 2
session <- 2  #session (i.e. learn or train): 1 = learn, 2 = train
tidx <- c(1, 20, 40, 60, 80)  #which trials you'd like to view within this context and session

# how would you like the figure to look?
title_sz <- 30
label_sz <- 30

### read the optimal path data get the working directory
project_path <- getwd()
# read the data
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "opt-path", sep = "_"), ".csv",
    sep = ""))
optimal <- read_csv(fnl, show_col_types = FALSE)

### read the observed path data read the data
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "evt", sep = "_"), ".csv", sep = ""))
observed <- read_csv(fnl, show_col_types = FALSE)
# re-code door ID as x and y grid positions
xloc <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
yloc <- c(4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
count <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
doors <- data.frame(xloc, yloc, id, count)
observed <- observed %>%
    mutate(x = xloc[door], y = yloc[door])


### plot!

# filter to just this subject, algorithm, context, and session
opt <- optimal %>%
    filter(sub == subject, algorithm == alg, context == ctx) %>%
    mutate(solution_factor = factor(solution))
obs <- observed %>%
    filter(sub == subject, context == ctx, ses == session, switch == 0)
trials <- unique(obs$t)
obs <- obs %>%
    filter(t %in% trials[tidx]) %>%
    mutate(t_factor = factor(t))

colours <- rep("white", 16)
idx <- unique(opt$door)
colours[idx] <- "lightgrey"

opt_other <- optimal %>%
    filter(sub == subject, algorithm == alg, context != ctx) %>%
    mutate(solution_factor = factor(solution))
idx <- unique(opt_other$door)
colours[idx] <- "darkgrey"


# make the figure
ggplot() + geom_tile(data = doors, aes(x = xloc, y = yloc, fill = id, colour = "white"), show.legend = FALSE,
    width = 0.9, height = 0.9, alpha = 1) + geom_text(data = doors, aes(x = xloc, y = yloc, label = id),
    size = 10) + geom_path(data = opt, aes(x = x, y = y, group = solution_factor), linewidth = 2, linejoin = "mitre",
    lineend = "butt", position = position_jitter(width = 0.1, height = 0.1), alpha = 0.8, arrow = arrow(angle = 15,
        type = "closed")) + geom_path(data = obs, aes(x = x, y = y, group = t_factor, colour = t_factor),
    linewidth = 2, linejoin = "mitre", lineend = "butt", position = position_jitter(width = 0.1, height = 0.1),
    alpha = 0.8, arrow = arrow(angle = 15, type = "closed")) + theme_minimal() + ylim(0.5, 4.5) + xlim(0.5,
    4.5) + scale_fill_gradientn(colours = colours, guide = "none") + scale_colour_brewer(palette = "BrBG",
    labels = c(tidx, "")) + labs(title = "Optimal and Observed Paths During Training", x = "Door Position (x)",
    y = "Door Position (y)", colour = "Trial") + theme(plot.title = element_text(size = title_sz), axis.text.x = element_text(size = label_sz),
    axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz), axis.title.x = element_text(size = label_sz),
    axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz))

# save it
fnl <- file.path(project_path, "fig", paste(paste(version, exp, session, mes, alg, "opt-path", paste("sub",
    subject, "context", ctx, sep = "-"), sep = "_"), ".png", sep = ""))
ggsave(fnl, plot = last_plot())
