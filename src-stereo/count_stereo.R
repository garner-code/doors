count_stereo <- function(data,opt,graph) {

    ### insensitive to feedback?  count re-clicks on previous context doors on switch trials
    events <- data %>%
        filter(switch == 1, door_oc == 1)
    reclicks <- events %>%
        group_by(sub, ses, t, context, subses) %>%
        summarise(n = n(), n_reclicks = n() - length(unique(door)))
    reclicks <- reclicks %>%
        group_by(sub, ses, context, subses) %>%
        summarise(clicks = mean(n), reclicks = mean(n_reclicks))

    ### accurate?  count accuracy on stay trials
    events <- data %>%
        filter(switch == 0)
    accuracy <- events %>%
        group_by(sub, ses, t, context, subses) %>%
        summarise(n_clicks = n(), n_correct = sum(door_cc), accuracy = n_correct/n_clicks)
    accuracy <- accuracy %>%
        group_by(sub, ses, context, subses) %>%
        summarise(accuracy = mean(accuracy))

    ### consistent in transitions?
    transitions <- data.frame(sub = integer(), ses = integer(), context = integer(), subses = integer(),
        transition_counts = double())
    for (su in unique(data$sub)) {
        for (se in unique(data$ses)) {
            for (co in unique(data$context)) {
                for (ss in unique(data$subses)) {
                  events <- data %>%
                    filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1, subses ==
                      ss)

                  # get their transitions
                  transition_counts <- matrix(0, nrow = 16, ncol = 16)
                  for (i in 2:nrow(events)) {
                    door <- events$door[i]
                    previous <- events$door[i - 1]
                    transition_counts[door, previous] <- 1  #yes, this transition happened. to count frequency: transition_counts[door,previous]+1
                  }

                  # sum the unique transitions
                  transition_counts <- rowSums(transition_counts)  #how many doors transition to this one?
                  transition_counts <- transition_counts[transition_counts != 0]  #ignore doors that were never transitioned to
                  transition_counts <- mean(transition_counts)

                  if (!is.nan(transition_counts)) {
                    # store
                    transitions[nrow(transitions) + 1, ] <- data.frame(su, se, co, ss, transition_counts)
                  }
                }
            }
        }
    }
    transitions_accuracy <- accuracy %>%
        add_column(transition_counts = transitions$transition_counts)

    ### following a shortest path?  load the shortest path data

    # for each subject, session, trial, and context...  
    #   check whether their clicks perfectly match any of the shortest paths 
    #   take the difference between their distance travelled (bw centre of
    # clicked doors, not true cursor position) and the distance under shortest path
    # note that this does not apply to session 3 for exp_lt, as the shortest paths only
    # describe context 1 and 2
    path_match <- data.frame(sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer(),
                              match = integer(), overshoot = double())
    for (su in unique(data$sub)) {
      for (se in c(1,2)) {
        for (co in unique(data$context)) {
          for (ss in unique(data$subses)) {

            # get clicks on stay trials
            events <- data %>%
              filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1, subses ==
                       ss)
            opt_sub <- opt %>% 
              filter(sub == su, context == co)
            
            # for each trial, check (a) whether their clicks match a shortest path, and (b) how much further they travelled
            for (tr in unique(events$t)){
              trial <- events %>% filter(t==tr)

                # do their clicks match a shortest path?
                match <- 0
                for (sol in unique(opt_sub$solution)){
                  solution <- opt_sub$door[opt_sub$solution==sol]
                  if (all(solution[1:length(trial$door)]==trial$door)){
                    match <- 1
                    break
                  }
                }
                
                # what's the difference between their distance travelled and the shortest path?
                trial <- trial %>% filter(door_cc==1)
                selected <- unique(trial$door)
                clicks <- trial$door
                
                # find a corresponding shortest path solution that matches as many doors as possible
                j <- min(c(length(clicks),4)) #if there are 4 or fewer clicks, take that number. otherwise, only consider first 4 clicks
                searching <- TRUE
                while(searching){
                  for (sol in unique(opt_sub$solution)){
                    solution <- opt_sub$door[opt_sub$solution==sol]
                    if (all(solution[1:j]==clicks[1:j])){
                      searching <- FALSE
                      break
                    }
                  }
                  if(searching==FALSE){break}else{j <- j-1}
                }
      
                # get the length of this solution
                shortest <- 0
                for (i in 2:length(solution)){
                  shortest <- shortest + graph[solution[i-1],solution[i]] #how quickly could they move between them?
                }
                
                # get the length of their path, adding unclicked doors in the shortest sequence
                travelled <- 0
                if (length(selected)<4){
                  doors <- c(clicks,solution[! solution %in% selected])
                }else{
                  doors <- clicks
                }
                for (i in 2:length(doors)){
                  travelled <- travelled + graph[doors[i-1],doors[i]] #how far did they travel?
                }
                overshoot <- travelled - shortest
                path_match[nrow(path_match) + 1, ] <- data.frame(su, se, tr, co, ss, match,overshoot)
                
            }
          }
        }
      }
    }

    return(list(reclicks, transitions_accuracy, path_match))
}
