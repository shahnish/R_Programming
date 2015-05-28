pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
    
    pollutantData <- double()  # empty double vector to store all the valid results
   
    ## First construct file name and path 
    for (i in seq_along(id))   ## Iterate through the vector and read each file
    {
        
        ## First create file name
        prefix <- "00"
        fileId <- id[i]
        if (fileId >= 100)
        {
            prefix <- ""
        }
        else if (fileId >= 10)
        {
            prefix <- "0"
        }
        else
        {
            prefix <- "00"
        }
        
        fileName <- paste(prefix,fileId, sep="") 
        fileName <- paste(fileName, ".csv", sep="")
        fileName <- paste(directory, fileName, sep="/")
        
        data <- read.csv(fileName, header=TRUE)
        
        readPollutant <- data[, pollutant]  # data from specific file and specific pollutant
        
        #remove NA and append valid data to pollutantData
        bad <- is.na(readPollutant)  # bad values
        good <- readPollutant[!bad]
        
        for (v in good)  ## This will get values
        {
            pollutantData <- c(pollutantData, v) 
        }
  
    }
    
    mean(pollutantData)

}