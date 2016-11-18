best <- function(state, outcome) {
        ## test parameters
        ##state <- "TX"
        ##outcome <- "heart attack"
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
        stateSubset <- subset(outcomeOfCare, State == state)
        
        ## we identify the best rate
        bestRate <- min(stateSubset[, rateCol], na.rm = TRUE) 
        
        ## We identify the best hospital (lowest 30-day death rate) 
        bestHosp <- c(subset(stateSubset, stateSubset[[rateCol]] == bestRate, select = Hospital.Name))
        c <- sort(as.vector(bestHosp$Hospital.Name))
        
        ## We return the hospital name in a character vector
        return(c[1])
        
}