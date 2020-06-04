outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
#need to do this section now >>> 2 Finding the best hospital in a state

best <- function(state) {
  ## Read outcome data 
  outcome[ , outcome$State == state]
  ## Check that state and outcome are valid
  
  ## Return hosputal name in that state with lowest 30-day death rate
  
}
