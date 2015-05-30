## Common reusable functions for Prog assignment 3

getOutcomeIndex <- function(outcome) {
    outcomeIndex <- if (outcome == "heart attack") 11
    else if (outcome == "heart failure") 17
    else if (outcome == "pneumonia") 23
    else NA
    
    outcomeIndex
}

# checkArgs <- function(data, state = NULL, outcome = NULL, num = NULL) {
#     if(is.null(data)) {
#         stop("no data")
#     }
#     
#     if (!is.null(state)) {
#         
#     }
# }