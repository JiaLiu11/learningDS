rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # read in data
    data_raw = read.csv("outcome-of-care-measures.csv")
    # find out if state and outcome are valid
    state_levels = levels(data_raw$State)
    outcome_levels = c("heart attack", "heart failure", "pneumonia")
    if(sum(state!=state_levels)<1){
        stop("invalid state")
    }
    if(outcome == outcome_levels[1]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }else if(outcome == outcome_levels[2]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }else if(outcome == outcome_levels[3]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }else{
        stop("invalid outcome")
    }
    outcome_data = data_raw[,c("State", "Hospital.Name", outcome_name)]
    outcome_state = outcome_data[outcome_data$State==state,] # data frame for a single state with single disease
    # clean NAs from state data
    outcome_state = outcome_state[outcome_state[outcome_name]!="Not Available",]
    # rank the state data according to outcome_name
    temp = outcome_state[[outcome_name]] # convert factor to numeric vector for ranking
    temp = levels(temp)[temp]
    outcome_state[outcome_name] = as.numeric(temp)
    sorted_data = outcome_state[order(outcome_state[,outcome_name], outcome_state[,"Hospital.Name"]), ]
    # find out if num is valid
    sorted_data_num = nrow(sorted_data)
    if(num=="best"){
        idx = 1
    }else if(num=="worst"){
        idx = sorted_data_num
    }else if(num>1 && num<sorted_data_num){
        idx = num
    }else{
        return(NA)
    }
    return(as.character(sorted_data[[idx, "Hospital.Name"]]))
}