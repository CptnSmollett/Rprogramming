corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    # 1. validate input - todo
    
    # 2. use complete() function
    source("complete.R")
    completeCases <- complete(directory)
    aboveThreshold <- completeCases[completeCases[["nobs"]] > threshold,]
    
    result = numeric(length(aboveThreshold[["id"]]))
    resIndex = 0L
    
    # 3. calculate correlations
    for (i in aboveThreshold[["id"]]) {
        # read values from file, filter complete cases, compute correlation using cor
        filename <- sprintf("%s/%03d.csv", directory, i)
        data <- read.csv(filename)
        resIndex <- resIndex + 1L
        
        cases <- complete.cases(data[["sulfate"]], data[["nitrate"]])
        result[resIndex] <- cor(data[["sulfate"]][cases], data[["nitrate"]][cases])
    }
    
    # 3. return result
    result
}