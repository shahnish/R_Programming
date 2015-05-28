rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validState = subset(data, State == state)
    if (nrow(validState) == 0)  ## no such state
    {
        stop("invalid state")
    }
    
    if ( ! ((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia")))
    {
        stop("invalid outcome")
    }
    
    if (outcome == "heart attack")
    {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"     ## heart attack = col. #11
    }
    else if (outcome == "heart failure") 
    {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
    }
    else if (outcome == "pneumonia")
    {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    
    #First get all data for that state
    stateData <- subset(data, State==state)
    
    #convert to numeric - also suppresses warnings
    stateData[, colName] <- suppressWarnings(as.numeric(stateData[, colName]))
    stateData[, c("Hospital.Name", colName, "State")]
    
    #order arranges first in first column then second column etc
    hospitalRanks <- stateData[order(stateData[, colName], stateData[, "Hospital.Name"], na.last=NA),c("Hospital.Name", colName, "State")]
    #head(hospitalRanks)
    
    if (num == "best")
    {
        rankedHospital <- hospitalRanks[1, "Hospital.Name"]
    }
    else if (num == "worst")
    {
        rankedHospital <- hospitalRanks[nrow(hospitalRanks), "Hospital.Name"]
    }
    else
    {
       rankedHospital <- hospitalRanks[num, "Hospital.Name"]  
    }
    
    rankedHospital
    

}
