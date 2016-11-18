rankall <- function(outcome, num = "best") {
        ## test parameters
        ##outcome <- "heart failure"
        ##num <- 4
        
        
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
        
        ## We subset the vector to contain only the hospital name, state and the selected rate
        outcomeOfCareSubset <- subset(outcomeOfCare, select = c(State, Hospital.Name,  rateCol))
        ## We sort the vector according to their rate and name in order to sort out ties 
        outcomeOfCareSubset <- outcomeOfCareSubset[order(outcomeOfCareSubset$State, outcomeOfCareSubset[3], outcomeOfCareSubset$Hospital.Name, na.last = NA),]
        
        
        ## For each state, find the hospital of the given rank
        ## The following conditions checks which rank should be returned
        ## and returns hospital namefor each state with the given rank
        hospRankByState <- aggregate(outcomeOfCareSubset, by = list(outcomeOfCareSubset$State), function(x){
                if (!is.numeric(num)){
                        if (num == "best") {
                                num <- 1
                        } else if (num == "worst") {
                                num <- length(x)
                        } else {
                                stop("invalid num")
                        }
                }
                
                x[num]
        })
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        retVal <- hospRankByState[, c(3, 1)]
        names(retVal) <- c("hospital", "state")
        
        return(retVal)

}