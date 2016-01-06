# functions for `anomalyDetection.Rmd`

fixInfinityByMean <- function(df, group_name, var_name, threshold){
  # Purpose: replace the inifinity (value>threshold) by the group mean

  # find the location of infinities
  inf_idx = which(abs(df[[var_name]])>threshold)
  if(length(inf_idx)==0){
    cat(c("fixInfinityByMean: no infinity found in", var_name, "!\n"))
    return(df)
  }
  # find group mean
  call = substitute(summarise(group_by_(df, group_name), mean=mean(var_name)), 
                    list(var_name=as.name(var_name)))
  df_mean_bygroup = eval(call)
  # loop over all inf_idx
  for(i in 1:length(inf_idx)){
    index_now = inf_idx[i]
    group_now = df[[index_now, group_name]]
    groupmean_now = df_mean_bygroup[which(df_mean_bygroup[[group_name]]==group_now),]$mean
    df[index_now, var_name] = groupmean_now
  }
  #cat(c(length(inf_idx), " infinities of ", var_name, "have been regulated!"))
  return(df)
}

calDerivative <- function(df, var_name, derv_name, groupEnd_name="group_end", step=1){
  # find the numeric derivative of var_name in df
  x_before = df[[var_name]]
  x_after  = x_before[-1]
  x_after[length(x_before)]=0 # out of range, set to zero
  df[[derv_name]] = (x_after - x_before)/step
  df[which(df[[groupEnd_name]]==1),derv_name]=0
  return(df)
}