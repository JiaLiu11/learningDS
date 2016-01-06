complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    completeTemp = matrix(nrow = length(id), ncol = 2)
    completeTemp[,1] = id
    for(i in 1:length(id)){
        fileName = file.path(directory, sprintf("%03d.csv", id[i]))
        if(file.exists(fileName)){
            data = read.csv(fileName)
            valid = sum(complete.cases(data))
            completeTemp[i,2] = valid
        } else{
            errorMsg = sprintf("No such file as %03d.csv", id[i])
            #        print errorMsg
            stop()
        }
    }
    completeResults = as.data.frame(completeTemp)
    colnames(completeResults) = c("id", "nobs")
    completeResults
}