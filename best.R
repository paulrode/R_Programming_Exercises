#Load Tidy enviroment 
library("tidyverse")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")

#Part 2 file: best.R
#need to do this section now >>> 2 Finding the best hospital in a state
best <- function(state, outcome) {
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  outcome1 <- outcome1[ , c(2,6,7,11, 17, 23)]
  names(outcome1) <- c("Hospital", "City", "State", "heart failure", "heart attack", "pneumonia")
  
  ## Check that state and outcome are valid
  if(any(state == outcome1$State, na.rm = TRUE) == "FALSE") { stop("invalid state" )}
  if(any(outcome == c("heart failure", "heart attack", "pneumonia"), na.rm = FALSE) == "FALSE") {
    stop("invalid outcome") } 
  
  ## Return hospital name in that state with lowest 30-day death rate
  filter(outcome1, State == state) %>% select(c("Hospital", outcome)) -> outcome1
  outcome1 %>% arrange(outcome1[ ,2], Hospital) -> outcome1
  print(outcome1[1:5,])
}



