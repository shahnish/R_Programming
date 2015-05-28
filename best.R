best <- function (state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    
    
    ## check that state and outcome are valid
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    #First get all data for that state
    stateData <- subset(data, State==state)
    
    #convert to numeric - also suppresses warnings
    stateData[, colName] <- suppressWarnings(as.numeric(stateData[, colName]))



    # get minimum. 
    min <- min(stateData[, colName], na.rm=TRUE)
    #print(min)
    
    #For this minimum, find the name of hospitals
    bestHospitals <- stateData[stateData[, colName] == min, "Hospital.Name"]
    #print(bestHospitals)
    bestHospitals <- sort(bestHospitals)
    bestHospitals[1]
}
