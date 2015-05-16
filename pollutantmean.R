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
    values <- numeric()
    
    for(i in id) {
        filename <- sprintf("%s/%03d.csv", directory, i)
        data <- read.csv(filename)
        
        values <- c(values, data[[pollutant]])
    }
    
    # 3. calculate mean
    mean(values, na.rm = TRUE)
}