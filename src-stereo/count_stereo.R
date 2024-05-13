count_stereo <- function(data, opt, graph) {

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
        transition_tril = double(), transition_counts = double())
    for (su in unique(data$sub)) {
        for (se in unique(data$ses)) {
            for (co in unique(data$context)) {
                for (ss in unique(data$subses)) {

                  # select stay trials
                  events <- data %>%
                    filter(switch == 0, sub == su, ses == se, context == co, subses == ss, door_cc ==
                      1)

                  # initialise a matrix
                  transition_counts <- matrix(0, nrow = 16, ncol = 16)

                  # select a trial
                  for (tr in unique(events$t)) {
                    trial <- events %>%
                      filter(t == tr)

                    # for all trials with more than one event, record door transitions
                    if (nrow(trial) > 1) {
                      for (i in 2:nrow(trial)) {
                        door <- trial$door[i]
                        previous <- trial$door[i - 1]
                        transition_counts[previous, door] <- 1  #yes, this transition happened
                      }
                    }
                  }

                  # clear the lower triangle of the matrix. this should ignore transitions in one
                  # direction
                  idx <- lower.tri(transition_counts, diag = FALSE)
                  transition_tril <- transition_counts
                  transition_tril[idx] <- 0

                  # for each door that was transitioned to, sum that column to get n-transitions
                  transition_tril <- colSums(transition_tril)
                  # discard any doors that were never selected, then take the mean
                  transition_tril <- mean(transition_tril[transition_tril != 0])

                  # repeat, now using the full matrix. this counts 1 -> 2 and 2 -> 1 as separate
                  # transitions
                  transition_counts <- colSums(transition_counts)
                  transition_counts <- mean(transition_counts[transition_counts != 0])

                  if (!is.nan(transition_counts)) {
                    # store
                    transitions[nrow(transitions) + 1, ] <- data.frame(su, se, co, ss, transition_tril,
                      transition_counts)
                  }
                }
            }
        }
    }
    transitions_accuracy <- accuracy %>%
        add_column(transition_tril = transitions$transition_tril, transition_counts = transitions$transition_counts)

    ### following a shortest path?  load the shortest path data

    # for each subject, session, trial, and context...  
    #   check whether their clicks perfectly match any of the shortest paths 
    #   take the difference between their distance travelled (bw centre of clicked doors, not true cursor position) 
    # and the distance under shortest path
    path_match <- data.frame(sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer(),match_tsp = double(),
                             travelled_tsp = double(),shortest_tsp = double(),overshoot_tsp = double(),match_hc = double(),
                             travelled_hc = double(),shortest_hc = double(),overshoot_hc = double())
    for (su in unique(data$sub)) {
        for (se in c(1, 2)) {
            for (co in unique(data$context)) {
                for (ss in unique(data$subses)) {

                  # get clicks on stay trials
                  events <- data %>% filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1, subses == ss)

                  # make the data frame
                  tmp <- data.frame(sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer())
                  for (tr in unique(events$t)){
                    tmp[nrow(tmp)+1,] <- data.frame(su,se,tr,co,ss)
                  }
                  
                  ### travelling salesman solutions (return to starting point)
                  opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "tsp")
                  df <- compare_paths(graph,events,opt_sub)
                  df_tsp <- df %>% rename(match_tsp = match,shortest_tsp = shortest, travelled_tsp = travelled, overshoot_tsp = overshoot)
                  
                  opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "hc")
                  df <- compare_paths(graph,events,opt_sub)
                  df_hc <- df %>% rename(match_hc = match,shortest_hc = shortest, travelled_hc = travelled, overshoot_hc = overshoot)

                  tmp <- cbind(tmp,df_tsp,df_hc)
                  
                  path_match <- rbind(path_match,tmp)
                }
            }
        }
    }

    return(list(reclicks, transitions_accuracy, path_match))
}

compare_paths <- function(graph,events,opt_sub){
  
  df <- data.frame(match = integer(), travelled = double(), shortest = double(), overshoot = double())
  
  # find what doors belonged to this context
  doors_cc <- unique(events$door)
  
  ### calculate the shortest path under this algorithm
  shortest <- 0
  for (sol in unique(opt_sub$solution)) {
    solution <- opt_sub$door[opt_sub$solution==sol]
    this_shortest <- 0
    for (i in 2:length(solution)){
      this_shortest <- this_shortest + graph[solution[i-1],solution[i]]
    }
    if (sol==1){
      shortest <- this_shortest
    }else{
      shortest <- min(c(shortest,this_shortest))
    }
  }

  ### loop through trials, testing whether they used the shortest path, and how much further they travelled
  for (tr in unique(events$t)) {
    trial <- events %>% filter(t == tr)
    
    # do their clicks match a shortest path?
    match <- 0
    for (sol in unique(opt_sub$solution)) {
      solution <- opt_sub$door[opt_sub$solution == sol]
      if (all(solution[1:length(trial$door)] == trial$door)) {
        match <- 1
        break
      }
    }
  
    #   if they've selected four doors, compare directly to the shortest path
    if(length(unique(trial$door))==4){
      travelled <- 0
      for(i in 2:length(trial$door)){
        travelled <- travelled + graph[trial$door[i-1],trial$door[i]]
      }
    }else{
      #   otherwise, find the most efficient way to end this sequence
      path <- trial$door #the doors they clicked, in the order they took (inc. re-clicks)
      unclicked <- doors_cc[!doors_cc %in% path]
      paths <- as_tibble(permutations(n = length(unclicked), r = length(unclicked), v = unclicked))
      d <- data.frame(matrix(unlist(path), nrow=1, byrow=FALSE))
      d <- do.call("rbind", replicate(nrow(paths), d, simplify = FALSE))
      paths <- cbind(d,paths)
      
      travelled <- 0
      for (i in 1:nrow(paths)){
        this_path <- paths %>% slice(i) %>% unlist(use.names=FALSE)
        this_travelled <- 0
        for(ii in 2:length(this_path)){
          this_travelled <- this_travelled + graph[this_path[ii-1], this_path[ii]]
        }
        if(i==1){
          travelled <- this_travelled
        }else{
          travelled <- min(c(travelled,this_travelled))
        }
      }
    }
    overshoot <- travelled - shortest
    df[nrow(df) + 1, ] <- data.frame(match,travelled,shortest,overshoot)
    
  }
  return(df)
}
