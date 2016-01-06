corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    id_list = 1:332
    completeCases = complete(directory, id_list)
    target_list = completeCases$id[completeCases$nobs>threshold]
    if(length(target_list)==0){
        result = vector(mode="numeric", length=0)
        return(result)
    }
    result = vector(mode="numeric", length=length(target_list))
    for (i in 1:length(target_list)) {
        fileName = file.path(directory, sprintf("%03d.csv", target_list[i]))
        if(file.exists(fileName)){
            data = read.csv(fileName)
            data_cleaned = data[complete.cases(data),]
            cor_local = cor(data_cleaned[["sulfate"]], data_cleaned[["nitrate"]])
            result[i] = cor_local
        } else{
            errorMsg = sprintf("No such file as %03d.csv", target_list[i])
            #        print errorMsg
            stop()
        }
    }
    result
}