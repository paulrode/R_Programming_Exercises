#Load Tidy enviroment 
library("tidyverse")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")

#Part 3  file: rankhospital.R
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  outcome1 <- outcome1[ , c(2,6,7,11, 17, 23)]
  names(outcome1) <- c("Hospital", "City", "State", "heart failure", "heart attack", "pneumonia")

  ## Check that state and outcome are valid
  if(any(state == outcome1$State, na.rm = FALSE) == "FALSE") { stop("invalid state" )}
  if(any(outcome == c("heart failure", "heart attack", "pneumonia"), na.rm = FALSE) == "FALSE") {
    stop("invalid outcome") } 
  
  ## Return hospital name in that state with the given rank
  filter(outcome1, State == state) %>% select(c("Hospital", outcome)) -> outcome1
  outcome1 %>% arrange(outcome1[ ,2], Hospital) -> outcome1
  if(num == "best") { num <- 1} 
  if(num == "worse") { num <- nrow(outcome1)}
  if(num > nrow(outcome1)) {print("NA")} else {print(outcome1[num, ])}
}



