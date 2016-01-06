# extracting features and implement anomaly detection

# 
library(RSQLite)
library(dplyr)
source('auxFunctions_AD.R')

featureExtraction <-function(db_name){
  # extract features which are ready to do anomaly dectection for all trips of 
  # one driver
  driver_db = src_sqlite(db_name)
  driver_sqlite=tbl(driver_db, "driver_data")
  coord = collect(select(driver_sqlite, x, y, trip_id))
  # find the end point of each group
  temp = coord$trip_id
  temp = temp[-1]
  temp[length(temp)+1]=201
  # transform and create group end indicator
  coord = mutate(coord, r=sqrt(x**2+y**2), theta=atan2(y,x), group_end = temp-trip_id)
  
  # find and regulate speed
  coord = calDerivative(coord, "r", "speed")
  coord = fixInfinityByMean(coord, "trip_id", "speed", 134)
  # find and regulate acceleration
  coord = calDerivative(coord, "speed", "acc")
  coord = fixInfinityByMean(coord, "trip_id", "acc", 11)  
  # find and regulate angular speed
  coord = calDerivative(coord, "theta", "angular_speed")
  coord = fixInfinityByMean(coord, "trip_id", "angular_speed", pi)
  # find angular acceleration
  coord = calDerivative(coord, "angular_speed", "angular_acc")
  # find number of hard breaks
  coord$hard_break = (coord$acc< -3.13)
  # find time spent on highway
  coord$on_highway = coord$speed>26.8
  
  # summarise to features
  trip_signature2 = coord %>% group_by(trip_id) %>%
    summarise(mean_speed = mean(speed), mean_acc = mean(acc), 
              mean_angular_speed=mean(angular_speed), mean_angular_acc=mean(angular_acc),
              trip_length= sum(speed), hardBreaks_10M= sum(hard_break)/trip_length*10,
              highway_time =sum(on_highway))
  trip_signature_scaled = transmute(trip_signature,
                                    mean_v=log(abs(mean_speed+1)), # +1 to avoid inf
                                    mean_a=log(abs(mean_acc+1)),
                                    mean_w=mean_angular_speed,
                                    mean_alpha=mean_angular_acc,
                                    s = log(trip_length),
                                    breaks=hardBreaks_10M,
                                    h_time=highway_time)# log(..+2) to avoid zeros
  return(trip_signature_scaled)
}



anomalyDetectionPerDriver <-function(driver_id){
  # implement Multi-variate Gaussian function to detect anomaly
  # ref to Andrew Ng.'s Machine Learning class
  db_filename = file.path("drivers_dbs", paste0(driver_id, ".db"))
  driver_signatures = featureExtraction(db_filename)
  feature_matrix = as.matrix(driver_signatures)
  mu = colMeans(feature_matrix)
  sigma = apply(feature_matrix, 2, sd)
  feature_matrix = scale(feature_matrix)
  # start anomaly detection 
  p_ad = rep(0,200)
  for(i in 1:200){
    x = feature_matrix[i,]
    p_vec = exp(-x**2/2.0)/sqrt(2*pi)
    p_ad[i] = prod(p_vec)
  }
  # scale
  p_ad = p_ad/max(p_ad)
  return(p_ad)
  }

drivers_all = as.integer(system('ls drivers', intern=T))
result_df=NULL
for(i in 1:length(drivers_all)){
  driver_now = drivers_all[i]
  driver_prob = anomalyDetectionPerDriver(driver_now)
  result_now = data.frame(driver_trip=paste(rep(driver_now,200),1:200, sep="_"),
                          prob = driver_prob)
  result_df = rbind(result_df, result_now)
  cat(paste(i, "/", length(drivers_all), "completes!\n"))
}
dumpfilename = "prediction_ad.csv"
write.csv(result_df, file=dumpfilename, row.names=F)
