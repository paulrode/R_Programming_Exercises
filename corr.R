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
corr <- function(directory, threshold = 0) 
{
  cr <- 0
  file_data <- 0
  files <- 0
  path <- 0
  path <- paste(getwd(), directory, sep = "/")
  files <-  list.files(path) 
  for (i in 1:length(files)) 
  {
    file_data <- read_csv(paste(path, files[i], sep = "/"))
    
    if ( threshold < nrow(file_data[complete.cases(file_data),]))
    {
      file_data <- file_data[complete.cases(file_data),]
      cr[i] <- cor(file_data$nitrate, file_data$sulfate) }    else {}
    }
  return(cr)
  }

#Run Function
cr <- corr("specdata",150)

