best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    # check validity of inputs
    state_valid = sum(state==levels(outcome_data$State))
    if(state_valid==0){
        stop("invalid state")
    }
    if(outcome == "heart attack"){
        outcome_disease = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"        
    } else if(outcome == "heart failure"){
        outcome_disease = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if(outcome == "pneumonia"){
        outcome_disease = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }else{
        stop("invalid outcome")
    }
    # find the index of max outcome
    outcome_state = outcome_data[outcome_data$State == state,]
    outcome_disease_temp = outcome_state[outcome_disease]
    outcome_disease_data = suppressWarnings(as.numeric(levels(unlist(outcome_disease_temp))[unlist(outcome_disease_temp)]))
    # eliminate possible NA
    outcome_disease_data[is.na(outcome_disease_data)] = max(outcome_disease_data, na.rm=T)
    min_idx = which(outcome_disease_data==min(outcome_disease_data))
    # find the state
    if(length(min_idx)==1){
        besthospital_name = outcome_state[[min_idx,"Hospital.Name"]]

    }else{
        besthospital_names = outcome_state[[min_idx,"Hospital.Name"]]
        besthospital_name = besthospital_names[rank(besthospital_names)[1]]
    }
    return(as.character(besthospital_name))    
}