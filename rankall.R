rankall <- function(outcome, num = "best") {
    
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    

    ## Check that outcome is valid
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    #convert to numeric - also suppresses warnings
    data[, colName] <- suppressWarnings(as.numeric(data[, colName]))
    
    ranked <- c()
    states <- c()
    
    # Get list of states. 
    for (state in sort(unique(data[, "State"])))
    {
        #First get all data for that state
        stateData <- subset(data, State==state)
        
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
        
        ranked <- c(ranked, rankedHospital)
        states <- c(states, state)
         
        #print(sprintf("Ranked: %s, State: %s", rankedHospital, state))
        
  
        
    }
    overall <- data.frame(ranked,states)
    colnames(overall) <- c("hospital", "state")
    overall
    
    
    
    
#     #First get all data for that state
#     stateData <- subset(data, State==state)
#     
#     #convert to numeric - also suppresses warnings
#     stateData[, colName] <- suppressWarnings(as.numeric(stateData[, colName]))
#     stateData[, c("Hospital.Name", colName, "State")]
#     
#     #order arranges first in first column then second column etc
#     hospitalRanks <- stateData[order(stateData[, colName], stateData[, "Hospital.Name"], na.last=NA),c("Hospital.Name", colName, "State")]
#     #head(hospitalRanks)
#     
#     if (num == "best")
#     {
#         rankedHospital <- hospitalRanks[1, "Hospital.Name"]
#     }
#     else if (num == "worst")
#     {
#         rankedHospital <- hospitalRanks[nrow(hospitalRanks), "Hospital.Name"]
#     }
#     else
#     {
#         rankedHospital <- hospitalRanks[num, "Hospital.Name"]  
#     }
#     
#     rankedHospital
    

}
