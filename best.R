## Prog assignment 3

source("commonProgAssgn3.R")

## function to find the best hospital in state

best <- function(state, outcome) {
    ## Read outcome data
    ## 2 - hospital name
    ## 7 - state - State
    ## 11 - heart attack
    ## 17 - heart failure
    ## 23 - pneumonia
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (match(state, data$State, nomatch = 0) == 0) {
        stop("invalid state")
    }
    if (outcome != "heart attack" &&
            outcome != "heart failure" &&
            outcome != "pneumonia")
    {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    outcomeIndex <- getOutcomeIndex(outcome)
    data[, outcomeIndex] <- as.numeric(data[, outcomeIndex])
    
    stateHospitals = which(data$State==state)
    minRate <- min(data[[outcomeIndex]][stateHospitals], na.rm = TRUE)
    
    hospital <- which(data$State==state & data[[outcomeIndex]]==minRate)
    
    data[[2]][hospital]
    
    ## df[which(df$Amount == min(df$Amount, na.rm = TRUE)), ]
}