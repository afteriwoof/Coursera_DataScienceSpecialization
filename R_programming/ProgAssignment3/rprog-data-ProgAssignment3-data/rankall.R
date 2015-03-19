df<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
df <- df[c(2, 7, 11, 17, 23)]
df[, c(3, 4, 5)] <- sapply(df[, c(3, 4, 5)], as.numeric)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  states <- unique(df$State)
  states <- sort(states)
  
  ranks <- data.frame(hospital=NA, state=NA)
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
  }
  
  ranks
}

