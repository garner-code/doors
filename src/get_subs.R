get_subs <- function(exp, version) {
  if (version == "pilot-data-00") {
    if (exp == "exp_ts") {
      subs <- c("sub-01", "sub-03")
    } else {
      subs <- c("sub-01", "sub-03")
    }
  } else if (version == "pilot-data-01") {
    if (exp == "exp_ts") {
      subs <- c("sub-04", "sub-08", "sub-20", "sub-22")
    } else {
      subs <- c("")
    }
  } else if (version == "pilot-data-02") {
    if (exp == "exp_ts") {
      subs <- c("sub-10", "sub-23")
    } else {
      # lt
      subs <- c("sub-11")
    }
  } else if (version == "study-01") {
    if (exp == "exp_ts") {
      subs <- c("")
    } else {
      subs <- c("sub-01", "sub-02", "sub-03", "sub-04", "sub-05", "sub-06", "sub-07", "sub-08", "sub-09", "sub-10", 
                "sub-11", "sub-12", "sub-13", "sub-14", "sub-15", "sub-16", "sub-17", "sub-18", "sub-19", "sub-20", 
                "sub-21", "sub-22", "sub-23", "sub-24", "sub-25", "sub-26", "sub-27", "sub-28", "sub-29", "sub-30")
    }
  }
}
