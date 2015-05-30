## Prog assignment 3

source("commonProgAssgn3.R")

## Function to get data frame with hospitals from all states with specified
## mortality rank
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## 2 - hospital name
    ## 7 - state - State
    ## 11 - heart attack
    ## 17 - heart failure
    ## 23 - pneumonia
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome and num are valid
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
    
    ## For each state, find the hospital of the given rank
    outcomeIndex <- getOutcomeIndex(outcome)
    data[[outcomeIndex]] <- as.numeric(data[[outcomeIndex]])
    
    states = sort(unique(data[[7]]))
    
    result <- data.frame(hospital = character(length(states)), state = states,
                         stringsAsFactors = FALSE,
                         row.names = states)
    
    for (i in 1:length(states)) {
        stateHospitals <- which(data$State == states[i])
        
        ranking <- order(data[[outcomeIndex]][stateHospitals],
                         data[[2]][stateHospitals],
                         na.last = NA)
        
        numRank <- if (num == "best") 1
                    else if (num == "worst") length(ranking)
                    else num
        result$hospital[i] <- if (numRank > length(ranking)) NA
                              else data[[2]][stateHospitals[ranking[numRank]]]
#         result$state[i] <- states[i]
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result
}