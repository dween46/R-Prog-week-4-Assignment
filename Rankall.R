# rankall

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  states <- unique(data$State)
  states <- states[order(states)]
  
  hospitals <- data.frame(hospital = character(), state = character())
  
  for (state in states) {
    hospital <- rankhospital(state, outcome, num)
    hospitals <- rbind(hospitals, data.frame(hospital = hospital, state = state))
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  hospitals
  
}