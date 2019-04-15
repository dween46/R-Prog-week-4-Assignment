best <- function(state, outcome) {
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
    data <- data[which(data$HeartAttack == min(data$HeartAttack, na.rm = TRUE)), ]
  } else if (outcome == "heart failure") {
    data <- data[which(data$HeartFailure == min(data$HeartFailure, na.rm = TRUE)), ]
  } else {
    data <- data[which(data$Pneumonia == min(data$Pneumonia, na.rm = TRUE)), ]
  }
  
  data <- data[order(data$Hospital), ]
  data$Hospital[[1]]
  
}