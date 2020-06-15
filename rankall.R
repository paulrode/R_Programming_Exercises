#Load Tidy enviroment 
library("tidyverse")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")

# Part 4  file: rankall.R
rankall <- function(outcome, num = "best") {
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  outcome1 <- outcome1[ , c(2,7,11, 17, 23)]
  names(outcome1) <- c("Hospital", "State", "heart failure", "heart attack", "pneumonia")
  
  ## Check that state and outcome are valid
  if(any(outcome == c("heart failure", "heart attack", "pneumonia"), na.rm = TRUE) == "FALSE") {
    stop("invalid outcome") } 
  
  ## Return hospital name in that state with the given rank
  outcome1 %>% select(c(Hospital, State, outcome)) -> outcome1
  as.tibble(outcome1) -> outcome1
  na.omit(outcome1) -> outcome1
  outcome1 %>% select(c(Hospital, State, outcome)) -> outcome1
  outcome1$Hospital <- as.character(outcome1$Hospital)
  outcome1$State <- as.character(outcome1$State)
  outcome1$`heart attack` <- as.numeric(outcome1$`heart attack`)
  unique(outcome1$State) -> outcome2
  if(num == "best") { num <- 1} 

  outcome1 %>%arrange(Hospital) %>%  group_by(State) %>% summarise( Name = nth(Hospital, num), Answer = nth(`heart attack` , num)) 

}



