corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    completeCount <- complete(directory)
    withThresholdApplied <- subset(completeCount, nobs > threshold)
    
    corrData <- double()  # empty double vector to store all the valid results
    
    if (nrow(withThresholdApplied) > 0)
    {
    
        for (i in seq_along(1:nrow(withThresholdApplied)))  # iterate through the threshholds
        {
    #       print(withThresholdApplied[i,"id"])
            # for each row, get station id, read file, do sulfate and nitrate corelation
            ## First create file name
            prefix <- "00"
            fileId <- withThresholdApplied[i,"id"]
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
            
            #get sulfate and nitrate values
            sulfate <- data[, "sulfate"]
            nitrate <- data[, "nitrate"]
            
            #Identify where both has values
            completeData <- complete.cases(sulfate, nitrate)
            
            #gather only valid values 
            sulfate <- sulfate[completeData]
            nitrate <- nitrate[completeData]
            
            #corelate
            corrData <- c(corrData, cor (sulfate, nitrate))
                 
        }
    }

    corrData
        

}