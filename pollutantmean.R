pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    # 1. validate input - todo
    
    # 2. read files
    pollutantSum <- 0
    pollutantCount <- 0
    
    for(i in id) {
        filename <- sprintf("%s/%03d.csv", directory, i)
        data <- read.csv(filename)
        
        valueIndices <- !is.na(data[pollutant])
        values <- data[pollutant][valueIndices]
        pollutantSum <- pollutantSum + sum(values)
        pollutantCount <- pollutantCount + length(values)
    } 
    
    # 3. calculate mean
    pollutantSum/pollutantCount
}