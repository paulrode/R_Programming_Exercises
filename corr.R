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
  i <- 1
  file_data <- 0
  files <- 0
  path <- 0
  path <- paste(getwd(), directory, sep = "/")
  files <-  list.files(path) 
  
  for (i in 1:length(files)) 
  {
    file_data <- read_csv(paste(path, files[i], sep = "/"))
    file_data <- file_data[complete.cases(file_data), ]
    
    if ( threshold < nrow(file_data))
    {
    file_data <- file_data[complete.cases(file_data), ]  
    cr[i] <- cor(file_data$nitrate, file_data$sulfate)
    }
  }
  cr <- na.omit(cr)
  return(cr[-1])
}

#Run Function
cr <- corr("specdata")
head(cr)
summary(cr)
length(cr)


cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)



cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
