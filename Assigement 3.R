#Load Tidy enviroment 
library("tidyverse")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")



outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
#need to do this section now >>> 2 Finding the best hospital in a state


best <- function(state, outcome) {
  
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #names(outcome1) <- c("Hospital", "City", "State", "Heart Failure", "Heart Attack", "Pneumonia")
 
  ## Check that state and outcome are valid
  any(state == outcome1$State, na.rm = TRUE)
  any(outcome == c(outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE)  
  
  ## Return hosputal name in that state with lowest 30-day death rate
  outcome1 <- outcome1[c(outcome1$State == state), c("Hospital.Name", "City", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                                                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )]
  outcome1
}
answer <- best("GA", "Heart Atack")
best("GA", "Heart Atack")

head(answer)

arrange(answer, `Heart Failure`)
