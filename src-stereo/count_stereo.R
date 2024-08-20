count_stereo <- function(exp, data, opt, graph) {
  utils::globalVariables(".data")

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
    summarise(n_clicks = n(), n_correct = sum(door_cc), n_correct_oc = sum(door_oc), 
              accuracy = n_correct / n_clicks, metatask_accuracy = (n_correct+n_correct_oc)/n_clicks)
  accuracy <- accuracy %>%
    group_by(sub, ses, context, subses) %>%
    summarise(accuracy = mean(accuracy), metatask_accuracy = mean(metatask_accuracy))

  ### consistent in transitions?
  transitions <- data.frame(
    sub = integer(), ses = integer(), context = integer(), subses = integer(),
    transition_counts = double(), transition_weights = double(), entropy = double()
  )
  print("getting the transition matrix")
  for (su in unique(data$sub)) {
    print(su)
    for (se in unique(data$ses)) {
      for (co in unique(data$context)) {
          for (ss in unique(data$subses)) {
            
            # reduce to correct click events on stay trials
            events <- data %>% filter(switch == 0, sub == su, ses == se, context == co, subses == ss)
  
            # -------------------------------------------------------------------------
            # make the full transitions matrix
            transition_matrix <- matrix(0, nrow = 16, ncol = 16)
            doors <- unique(events$door)
            
            # select a trial
            for (tr in unique(events$t)) {
              trial <- events %>% filter(t == tr)
              
              # if there's more than one event, record door transitions
              if (nrow(trial) > 1) {
                for (i in 2:nrow(trial)) {
                  door <- trial$door[i]
                  previous <- trial$door[i - 1]
                  transition_matrix[previous, door] <- transition_matrix[previous, door]+1 
                }
              }
            }
            
            # -------------------------------------------------------------------------
            # TRANSITION COUNTS
            # for each door i, find the number of unique ways that this person gets to it, then take the mean across i's
            transition_counts <- colSums(transition_matrix)
            transition_counts <- mean(transition_counts[transition_counts!=0])
            
            # -------------------------------------------------------------------------
            # TRANSITION WEIGHTS
            # for each door i, find the door j that most often goes to it. take its probability (n clicks on j before i / n clicks on i)
            transition_weights <- colMax(data.frame(transition_matrix))/colSums(transition_matrix)
            transition_weights <- mean(transition_weights[!is.na(transition_weights)])
  
            # -------------------------------------------------------------------------
            # ENTROPY
            # take the probability of each transition, given the number of transitions
            # multiply by the log of its probability, sum log probabilities, and take the negative
            entropy <- transition_matrix/sum(transition_matrix)
            entropy <- entropy * sapply(entropy,log2)
            entropy <- -mean(entropy,na.rm=TRUE)
            
            if (!is.nan(transition_counts)) {
              # store
              transitions[nrow(transitions) + 1, ] <- data.frame(su, se, co, ss, transition_counts, transition_weights, entropy)
            }
          }
        }
    }
  }
  transitions_accuracy <- accuracy %>% add_column(transition_counts = transitions$transition_counts, 
                                                  transition_weights = transitions$transition_weights,
                                                  entropy = transitions$entropy)

  ### following a shortest path?
  path_match <- data.frame(
    sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer(), travelled = double(),
    travelling_match = double(), travelling_overshoot = double(), hamiltonian_match = double(), hamiltonian_overshoot = double()
  )
  print("processing match to shortest path")
  for (su in unique(data$sub)) {
    print(su)
    for (se in c(2)) {
      for (co in unique(data$context)) {
        for (ss in unique(data$subses)) {
          # get clicks on stay trials
          events <- data %>% filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1, subses == ss)

          # make the data frame
          tmp <- data.frame(sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer())
          for (tr in unique(events$t)) {
            tmp[nrow(tmp) + 1, ] <- data.frame(su, se, tr, co, ss)
          }

          ### travelling salesman solutions (return to start)
          opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "travelling")
          df <- compare_paths(graph, events, opt_sub, "travelling")
          df_tsp <- df %>% rename(travelling_match = match, travelling_overshoot = overshoot)

          ### shortest hamiltonian path (don't return to start)
          opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "hamiltonian")
          df <- compare_paths(graph, events, opt_sub, "hamiltonian")
          df_hp <- df %>% rename(hamiltonian_match = match, hamiltonian_overshoot = overshoot) %>% select(hamiltonian_match,hamiltonian_overshoot)

          # stack
          tmp <- cbind(tmp, df_tsp, df_hp)
          path_match <- rbind(path_match, tmp)
        }
      }
    }
  }

  # put all these measures together
  path_match <- path_match %>%
    group_by(sub, ses, context, subses) %>%
    summarise_all(mean) %>% select(!t)
  t <- transitions_accuracy %>%
    ungroup() %>%
    select(accuracy, metatask_accuracy, transition_counts, transition_weights, entropy)
  r <- reclicks %>%
    ungroup() %>%
    select(clicks, reclicks)
  
  stereo <- bind_cols(path_match, t, r)

  return(stereo)
}

colMax <- function(data) {
  sapply(data, max, na.rm = TRUE)
}
compare_paths <- function(graph, events, opt_sub, alg) {
  df <- data.frame(travelled = double(), match = double(), overshoot = double())

  # find what doors belonged to this context
  doors_cc <- unique(events$door)

  # get the shortest path under this algorithm
  shortest <- opt_sub$path_weight[1]
  
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
    path <- trial$door # the doors they clicked, in the order they took (inc. re-clicks)
    if (length(unique(path)) == 4) {
      if (alg == "travelling") {
        path <- c(path, path[1])
      } # close the loop!
      travelled <- 0
      for (i in 2:length(path)) {
        travelled <- travelled + graph[path[i - 1], path[i]]
      }
    } else {
      #   otherwise, find the most efficient way to end this sequence
      unclicked <- doors_cc[!doors_cc %in% path]
      paths <- as_tibble(permutations(n = length(unclicked), r = length(unclicked), v = unclicked))
      d <- matrix(unlist(path), nrow = 1, byrow = FALSE)
      d <- data.frame(d)
      d <- do.call("rbind", replicate(nrow(paths), d, simplify = FALSE))
      if (alg == "travelling") {
        f <- matrix(path[1], nrow = 1, byrow = FALSE)
        f <- data.frame(f)
        f <- do.call("rbind", replicate(nrow(paths), f, simplify = FALSE))
        paths <- cbind(d, paths, f)
      } else {
        paths <- cbind(d, paths)
      }

      travelled <- 0
      for (i in 1:nrow(paths)) {
        this_path <- paths %>%
          slice(i) %>%
          unlist(use.names = FALSE)
        this_travelled <- 0
        for (ii in 2:length(this_path)) {
          this_travelled <- this_travelled + graph[this_path[ii - 1], this_path[ii]]
        }
        if (i == 1) {
          travelled <- this_travelled
        } else {
          travelled <- min(c(travelled, this_travelled))
        }
      }
    }
    travelled <- round(travelled,4)
    shortest <- round(shortest,4)
    
    overshoot <- travelled - shortest
    df[nrow(df) + 1, ] <- data.frame(travelled, match, overshoot)
  }
  return(df)
}
