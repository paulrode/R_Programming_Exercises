
#Load Tidy enviroment 
library("tidyverse")
library("dplyr")
library("stringr")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")


complete <- function(directory, id=1:332) 
  {
  
    file_data <- 0
    files <- 0
    path <- 0
    path <- paste(getwd(), directory, sep = "/")
    files <-  list.files(path) 
    for (i in 1:length(id)) 
    {
        file_data <- read_csv(paste(path, files[i], sep = "/"))
        nobs[i] <- nrow(file_data[complete.cases(file_data),])
    }
    answer <- data.frame(id,nobs)
    return(answer)
  }

complete(specdata, 1:10)




