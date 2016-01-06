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
    col = vector()
    for(i in 1:length(id)){
        fileName = file.path(directory, sprintf("%03d.csv", id[i]))
        if(file.exists(fileName)){
            data = read.csv(fileName)
            col = c(col, data[[pollutant]])
        } else{
            errorMsg = sprintf("No such file as %03d.csv", id[i])
            #        print errorMsg
            stop()
        }
    }
    meanValue = mean(col,na.rm=TRUE)
    meanValue
}