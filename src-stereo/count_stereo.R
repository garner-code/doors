count_stereo <- function(data, opt, graph) {
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
    summarise(n_clicks = n(), n_correct = sum(door_cc), accuracy = n_correct / n_clicks)
  accuracy <- accuracy %>%
    group_by(sub, ses, context, subses) %>%
    summarise(accuracy = mean(accuracy))

  ### consistent in transitions?
  transitions <- data.frame(
    sub = integer(), ses = integer(), context = integer(), subses = integer(),
    transition_counts = double()
  )
  for (su in unique(data$sub)) {
    for (se in unique(data$ses)) {
      for (co in unique(data$context)) {
        for (ss in unique(data$subses)) {
          # select stay trials
          events <- data %>% filter(switch == 0, sub == su, ses == se, context == co, subses == ss, door_cc == 1)

          # initialise a matrix
          transition_counts <- matrix(0, nrow = 16, ncol = 16)

          # select a trial
          for (tr in unique(events$t)) {
            trial <- events %>% filter(t == tr)

            # for all trials with more than one event, record door transitions
            if (nrow(trial) > 1) {
              for (i in 2:nrow(trial)) {
                door <- trial$door[i]
                previous <- trial$door[i - 1]
                transition_counts[previous, door] <- 1 # yes, this transition happened
              }
            }
          }
          transition_counts <- colSums(transition_counts)
          transition_counts <- mean(transition_counts[transition_counts != 0])

          if (!is.nan(transition_counts)) {
            # store
            transitions[nrow(transitions) + 1, ] <- data.frame(su, se, co, ss, transition_counts)
          }
        }
      }
    }
  }
  transitions_accuracy <- accuracy %>% add_column(transition_counts = transitions$transition_counts)

  ### following a shortest path?
  path_match <- data.frame(
    sub = integer(), ses = integer(), t = integer(), context = integer(), subses = integer(), match_tsp = double(),
    travelled_tsp = double(), shortest_tsp = double(), overshoot_tsp = double(), match_hp = double(),
    travelled_hp = double(), shortest_hp = double(), overshoot_hp = double()
  )
  for (su in unique(data$sub)) {
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
          opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "tsp")
          df <- compare_paths(graph, events, opt_sub, "tsp")
          df_tsp <- df %>% rename(match_tsp = match, shortest_tsp = shortest, travelled_tsp = travelled, overshoot_tsp = overshoot)

          ### shortest hamiltonian path (don't return to start)
          opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "hp")
          df <- compare_paths(graph, events, opt_sub, "hp")
          df_hp <- df %>% rename(match_hp = match, shortest_hp = shortest, travelled_hp = travelled, overshoot_hp = overshoot)

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
    select(accuracy, transition_counts)
  r <- reclicks %>%
    ungroup() %>%
    select(clicks, reclicks)
  stereo <- bind_cols(path_match, t, r)

  return(stereo)
}

compare_paths <- function(graph, events, opt_sub, alg) {
  df <- data.frame(match = integer(), travelled = double(), shortest = double(), overshoot = double())

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
      if (alg == "tsp") {
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
      if (alg == "tsp") {
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
    overshoot <- travelled - shortest
    df[nrow(df) + 1, ] <- data.frame(match, travelled, shortest, overshoot)
  }
  return(df)
}
