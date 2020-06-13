#Load Tidy enviroment 
library("tidyverse")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")


#Part 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])



#Part 2 file: best.R
#need to do this section now >>> 2 Finding the best hospital in a state


best <- function(state, outcome) {
  
  ## Read outcome data 
  outcome1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  outcome1 <- outcome1[ , c(2,6,7,11, 17, 23)]
  names(outcome1) <- c("Hospital", "City", "State", "heart failure", "heart attack", "pneumonia")
 
  ## Check that state and outcome are valid
  if(any(state == outcome1$State, na.rm = TRUE) == "FALSE") { stop("invalid state" )}
  if(any(outcome == c("heart failure", "heart attack", "pneumonia"), na.rm = TRUE) == "FALSE") {
    stop("invalid outcome") } 
  
  ## Return hospital name in that state with lowest 30-day death rate
  filter(outcome1, State == state) %>% select(c("Hospital", outcome)) %>% na.omit()-> outcome1
  outcome1 %>% arrange(outcome1[ ,2], Hospital) -> outcome1
  print(outcome1[1:5,])
  }

best("TX", "heart attack")





#Part 3  file: rankhospital.R

rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data 
    outcome1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
    outcome1 <- outcome1[ , c(2,6,7,11, 17, 23)]
    names(outcome1) <- c("Hospital", "City", "State", "heart failure", "heart attack", "pneumonia")

  ## Check that state and outcome are valid
    if(any(state == outcome1$State, na.rm = TRUE) == "FALSE") { stop("invalid state" )}
    if(any(outcome == c("heart failure", "heart attack", "pneumonia"), na.rm = TRUE) == "FALSE") {
    stop("invalid outcome") } 

  ## Return hospital name in that state with the given rank
    filter(outcome1, State == state) %>% select(c("Hospital", outcome)) %>% na.omit()-> outcome1
    outcome1 %>% arrange(outcome1[ ,2], Hospital) -> outcome1
    if(num == "best") { num <- 1} 
    if(num == "worse") { num <- nrow(outcome1)}
    if(num > nrow(outcome1)) {print("NA")} else {print(outcome1[num, ])}
    
    }

  
rankhospital("MD", "heart failure", "worse")
rankhospital("MN", "heart attack", 5000)





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
    outcome1 %>% select(c(Hospital, State, outcome)) %>% na.omit() -> outcome1
    outcome1 %>% group_by(State) %>% 
      summarise(Rank = min(outcome1[ , 2]))
    
    summarise(outcome1, (outcome1$`heart attack`[num])) -> answer
    view(answer)
    
}


rankall("heart attack", 2)


