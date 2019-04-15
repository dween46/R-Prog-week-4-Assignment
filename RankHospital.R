# rankhospital

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data <- data[, c("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  colnames(data) <- c("State", "Hospital", "HeartAttack", "HeartFailure", "Pneumonia")
  data <- data[data$State == state, ]
  
  ## change class
  data[, 3] <- as.numeric(data[, 3])
  data[, 4] <- as.numeric(data[, 4])
  data[, 5] <- as.numeric(data[, 5])
  
  if (outcome == "heart attack") {
    data <- data[order(data$HeartAttack, data$Hospital), ]
    data <- data[complete.cases(data[, 3]), ]
  } else if (outcome == "heart failure") {
    data <- data[order(data$HeartFailure, data$Hospital), ]
    data <- data[complete.cases(data[, 4]), ]
  } else {
    data <- data[order(data$Pneumonia, data$Hospital), ]
    data <- data[complete.cases(data[, 5]), ]
  }
  
  data <- cbind(data, Rank = 1:nrow(data))
  
  if (num == "best") {
    data <- data[data$Rank == 1, ]
  } else if (num == "worst") {
    data <- data[data$Rank == nrow(data), ]
  } else if (num > 1 && num < nrow(data)) {
    data <- data[data$Rank == num, ]
  } else {
    return(NA)
  }
  
  data$Hospital
  
}