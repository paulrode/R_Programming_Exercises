pollutantmean <- function(directory, pollutant, id=1:332) {
  
  file_data <- 0
  files <- 0
  path <- 0
  path <- paste(getwd(), directory, sep = "/")
  files <-  list.files(path) 
  for (i in id[i]:length(id)) 
  {
    if (i == 1) {
      file_data <- read_csv(paste(path, files[71], sep = "/"))
    } else {
      file_data <- rbind(file_data, read_csv(paste(path, files[i], sep = "/")))
    }
  }
  file_output <- file_data
  sulfate <- file_output$sulfate
  nitrate <- file_output$nitrate
  if (pollutant == "sulfate") {
    return(mean(sulfate, na.rm = TRUE))
  } else {
    return(mean(nitrate, na.rm = TRUE))
  }
}

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")
