count_stereo <- function(data) {

    ### insensitive to feedback?  count re-clicks on previous context doors on switch trials
    events <- data %>%
        filter(switch == 1, door_oc == 1)
    reclicks <- events %>%
        group_by(sub, ses, t, context, subses) %>%
        summarise(n = n(), n_reclicks = n() - length(unique(door)))
    reclicks <- reclicks %>%
        group_by(sub, ses, context, subses) %>%
        summarise(
            clicks = mean(n),
            reclicks = mean(n_reclicks)
        )

    ### accurate?  count accuracy on stay trials
    events <- data %>%
        filter(switch == 0)
    accuracy <- events %>%
        group_by(sub, ses, t, context, subses) %>%
        summarise(
            n_clicks = n(), n_correct = sum(door_cc),
            accuracy = n_correct/n_clicks
        )
    accuracy <- accuracy %>%
        group_by(sub, ses, context, subses) %>%
        summarise(accuracy = mean(accuracy))

    ### consistent in transitions?
    transitions <- data.frame(sub = integer(), ses = integer(), context = integer(), subses = integer(), transition_counts = double())
    for (su in unique(data$sub)) {
        for (se in unique(data$ses)) {
            for (co in unique(data$context)) {
                for (ss in unique(data$subses)){
                  events <- data %>%
                    filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1, subses == ss)
                  
                  # get their transitions
                  transition_counts <- matrix(0, nrow = 16, ncol = 16)
                  for (i in 2:nrow(events)) {
                    door <- events$door[i]
                    previous <- events$door[i - 1]
                    transition_counts[door, previous] <- 1  #yes, this transition happened. to count frequency: transition_counts[door,previous]+1
                  }
                  
                  # sum the unique transitions
                  transition_counts = rowSums(transition_counts)  #how many doors transition to this one?
                  transition_counts <- transition_counts[transition_counts!=0] #ignore doors that were never transitioned to
                  transition_counts <- mean(transition_counts)
                  
                  if (!is.nan(transition_counts)){
                    # store
                    transitions[nrow(transitions) +
                                  1, ] <- data.frame(su, se, co, ss, transition_counts)
                  }
                }
            }
        }
    }
    transitions_accuracy <- accuracy %>%
        add_column(transition_counts = transitions$transition_counts)
    
    ###
    # following a shortest path?
    # load the shortest path data
    
    # for each subject, session, trial, and context...
    #   check whether their clicks perfectly match any of the shortest paths
    #   take the difference between their distance travelled (bw centre of clicked doors, not true cursor position) and the distance under shortest path
    path_match <- c('')
    
    return(list(reclicks, transitions_accuracy, path_match))
}
