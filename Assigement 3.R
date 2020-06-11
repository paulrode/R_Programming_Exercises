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
  
  state <- "GA"
  outcome <- "Pneumonia"
  
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome1 <- outcome1[ , c(2,6,7,11, 17, 23)]
  names(outcome1) <- c("Hospital", "City", "State", "Heart Failure", "Heart Attack", "Pneumonia")
 
  ## Check that state and outcome are valid
 
  if(any(state == outcome1$State, na.rm = TRUE) == "FALSE") { stop("invalid state" )}
  if(any(outcome == c("Heart Failure", "Heart Attack", "Pneumonia"), na.rm = TRUE) == "FALSE") {
    stop("invalid outcome") } 
  
  
  ## Return hosputal name in that state with lowest 30-day death rate
  filter(outcome1, State == state) %>% select(Hospital, City, State, outcome) %>% na.omit() -> outcome2
  as.numeric(outcome2[,4]) -> outcome2[ ,4]
  na.omit(outcome2)
  outcome2 %>% arrange(outcome2[ , 4]) %>% arrange(outcome2[ , 1]) -> outcome2
  print(outcome2[1,])
  }
answer <- best("GA", "Pneumonia")

answer

head(answer) 


