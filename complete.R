
#Load Tidy enviroment 
library("tidyverse")
library("dplyr")
library("stringr")
library("lubridate")
library("knitr")
library("kableExtra")
library("fpp2")
library("readxl")

#Define Function
complete <- function(directory, id = 1:332) 
  {
    answer <- 0
    nobs <- 0
    file_data <- 0
    files <- 0
    path <- 0
    path <- paste(getwd(), directory, sep = "/")
    files <-  list.files(path) 
    for (i in 1:length(id)) 
    {
        file_data <- read_csv(paste(path, files[id[i]], sep = "/"))
        nobs[i] <- nrow(file_data[complete.cases(file_data),])
    }
    answer <- data.frame(id,nobs)
    return(answer)
  }

#Run Function
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


