rankhospital <- function(state, outcome, num = "best") {
        ## test parameters
        ## state <- "TX"
        ## outcome <- "heart failure"
        ## num <- 4
        ##bestRate <- 12.9
        
        
        ## Check that outcome is valid and define the rate columns
        if (outcome == "heart attack"){
                rateCol <- 11
        } else if (outcome == "heart failure"){
                rateCol <- 17
        } else if (outcome =="pneumonia"){
                rateCol <- 23
        } else {
                stop("invalid outcome")
        }
        
        ## Read outcome data
        outcomeOfCare <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## The selected rate column is converted to numeric
        outcomeOfCare[, rateCol] <- suppressWarnings(as.numeric(outcomeOfCare[, rateCol]))  
        
        ## Check that state is valid
        uniqueStates <- unique(outcomeOfCare[, 7])
        
        if (!state %in% uniqueStates) {
                stop("invalid state")
        }
        
        ## we subset the dataframe to the requested state
        stateSubset <- subset(outcomeOfCare, State == state, select = c(Hospital.Name, rateCol))
        
        ## We create an ordered vector to be used in sorting hospitals according to their rate 
        ## and name in order to sort out ties 
        orderedStateSubset <- order(stateSubset[2], stateSubset$Hospital.Name, na.last = NA)
        
        ## The following conditions checks which rank should be returned
        ## and returns hospital name in that state with the given rank
        if (num == "best") {
                as.character(stateSubset$Hospital.Name[orderedStateSubset[1]])
        } else if (num == "worst") {
                as.character(stateSubset$Hospital.Name[orderedStateSubset[length(orderedStateSubset)]])
        } else if (is.numeric(num)) {
                as.character(stateSubset$Hospital.Name[orderedStateSubset[num]])
        } else {
                stop("invalid num")
        }
        
}