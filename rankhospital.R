## Prog assignment 3

source("commonProgAssgn3.R")

## Function to get a hospital with specified mortality rank in a specified state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## 2 - hospital name
    ## 7 - state - State
    ## 11 - heart attack
    ## 17 - heart failure
    ## 23 - pneumonia
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    stateHospitals = which(data$State == state)
    
    ## Check that state, outcome and num are valid
    if (length(stateHospitals) == 0) {
        stop("invalid state")
    }
    if (outcome != "heart attack" &&
            outcome != "heart failure" &&
            outcome != "pneumonia")
    {
        stop("invalid outcome")
    }
    if ((is.character(num) && num != "best" && num != "worst") ||
            (is.integer(num) && num <= 0)) {
        stop("invalid num")
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    outcomeIndex <- getOutcomeIndex(outcome)
    data[[outcomeIndex]] <- as.numeric(data[[outcomeIndex]])
    
    ## order hospitals by mortality rate
    ranking <- order(data[[outcomeIndex]][stateHospitals],
                     data[[2]][stateHospitals],
                     na.last = NA)
    
    numRank <- if (num == "best") 1
    else if (num == "worst") length(ranking)
    else num
    if (numRank > length(ranking)) {
        return(NA)
    }
    
    ## select hospital with needed rank
    data[[2]][stateHospitals[ranking[numRank]]]
}