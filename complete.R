complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    completeCount <- data.frame(id=integer(),nobs=integer())
    
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
        

        sulfate <- data[, "sulfate"]
        nitrate <- data[, "nitrate"]
        completeData <- complete.cases(sulfate, nitrate)
        newRow <- c(id[i], length(completeData[completeData == TRUE]))
        completeCount <- rbind(completeCount, newRow)   ## removes column names from data frame if its 0 length and assignes first column names
                

        
    }
    colnames(completeCount) <- c("id", "nobs")
    completeCount


}