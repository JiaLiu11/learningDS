rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data_raw = read.csv("outcome-of-care-measures.csv")
    outcome_levels = c("heart attack", "heart failure", "pneumonia")
    if(outcome == outcome_levels[1]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }else if(outcome == outcome_levels[2]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }else if(outcome == outcome_levels[3]){
        outcome_name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }else{
        stop("invalid outcome")
    }
    # get rid of NAs
    data_cleaned = data_raw[data_raw[outcome_name]!="Not Available",]
    data_need = data_cleaned[,c("State", "Hospital.Name", outcome_name)]
    # transform to numeric
    outcome_data = data_need[[outcome_name]]
    outcome_data = as.numeric(levels(outcome_data)[outcome_data])
    data_need[[outcome_name]]=outcome_data
    # split by state for ranking
    data_split=split(data_need,data_need$State)
    states_total = length(data_split)
    
    # rank hospitals state by state
    result_dataframe = data.frame("Hospital.Name"=NA, "State" = "NONE")
    for (i in seq_len(states_total)){
        outcome_state = as.data.frame(data_split[i])
        state_name = colnames(outcome_state)[1]
        hospital_name = colnames(outcome_state)[2]
        outcome_name_now = colnames(outcome_state)[3]
        outcome_state_ordered = outcome_state[order(outcome_state[,outcome_name_now], outcome_state[,hospital_name]), ]
        outcome_state_ordered[["Rank"]]=seq_len(nrow(outcome_state))
        
        # find the specific rank hospital
        outcome_state_ordered_cleaned = subset(outcome_state_ordered, select=c(hospital_name, state_name))
        sorted_data_num = nrow(outcome_state_ordered)
        state_name_clean = strsplit(state_name,"[.]")[[1]][1]
        if(num=="best"){
            idx = 1
        }else if(num=="worst"){
            idx = sorted_data_num
        }else{
            idx = num
        }
        if(sum(outcome_state_ordered$Rank==idx)==0){
            temp_state = data.frame("Hospital.Name"=NA, "State" = state_name_clean)
        }else{
            temp_state = outcome_state_ordered_cleaned[outcome_state_ordered$Rank==idx,]
            # clean up column names
            colnames(temp_state)<-c("Hospital.Name","State")
        }
        result_dataframe = rbind(result_dataframe,temp_state)
    }
        # clean up the result
        state_column = result_dataframe$State
        state_list = levels(state_column)[state_column] # convert factor column to vector
        row.names(result_dataframe) = state_list  # modify row names to state names
        colnames(result_dataframe) = c("hospital","state")    # modify column names
        result_dataframe = result_dataframe[-1,]         #delete the first line
        result_dataframe  
}