count_paths <- function(clicks, start_point = "fixed"){
  
  # define the six possible paths, assuming that people loop, so we can ignore start point
  routines <- list(c(1,2,3,4),c(1,2,4,3),c(1,3,2,4),c(1,3,4,2),c(1,4,2,3),c(1,4,3,2))
  
  # find their context-correct doors, and re-name them [1 2 3 4]
  doors <- clicks %>% pull(door) %>% unique
  
  # opt 1: assume that everyone starts in the top left of the grid
  if(start_point=="fixed"){
    a <- c(doors[!is.na(match(doors,c(1,2,5,6)))],
           doors[!is.na(match(doors,c(9,10,13,14)))],
           doors[!is.na(match(doors,c(11,12,15,16)))],
           doors[!is.na(match(doors,c(3,4,7,8)))])
  }else if(start_point=="favourite"){
    # opt 2: let them start in their favourite spot
    favourite <- clicks %>% 
      mutate(t_diff = diff(c(0,t))) %>% 
      filter(t_diff == 1, door_cc == 1) %>% 
      pull(door) %>% 
      Mode
    a <- c(doors[!is.na(match(doors,favourite))], doors[is.na(match(doors,favourite))])
  }
  clicks <- clicks %>% mutate(door_id = case_when(door==a[1]~1,door==a[2]~2,door==a[3]~3,door==a[4]~4))

  # through a sliding window of four unique clicks, count which of these it maps onto
  counts <- matrix(0,length(routines))
  click <- 1
  while (click < nrow(clicks)-4){

    # find the clicks that land on the top left door
    if(clicks$door_id[click]==1){

      # store information about that click
      sample <- clicks[click,]
      
      # hunt for the next three UNIQUE clicks
      idx <- click
      while(nrow(sample)<4){
        if(is.na(match(clicks$door_id[idx],sample$door_id))){
          sample <- rbind(sample,clicks[idx,])
        }else if (idx == nrow(clicks)){
          break
        }
        idx <- idx+1
      }
      click <- idx
      
      # compare to the six possible routines
      routine <- which(sapply(routines, function(x,y) all(x==y) , y=sample$door_id))
      counts[routine] <- counts[routine]+1
      
    }else{
      click <- click+1
    }
  }

  counts <- data.frame(counts)
  counts$r <- paste("r", 1:nrow(counts), sep="")
  return(counts)
}