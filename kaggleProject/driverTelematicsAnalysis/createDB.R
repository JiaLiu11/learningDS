# filename:  createDB.R
#
# purpose:   merge all the files into a data frame and write to a data base for
#            easy reading and manipulation.

# clean start
rm(list=ls())

# load library
library(data.table) # for fread()
library(dplyr)      # for data manipulation
library(RSQLite)    # for database connection

readOneDriverOneTrip <- function(trip_idx, driver_idx){
  # read in data for one trip of a specific driver
  perTrip = fread(file.path('drivers',driver_idx,paste0(trip_idx,".csv")),
                  header=T, sep=",") # faster than read.csv
  perTrip = mutate(perTrip, driver_id = driver_idx, trip_id = trip_idx)
  return(perTrip)
}

readAllDrivers <- function(driver_id_list){
# wrapper of readOneDriverOneTrip(), read all drivers's trips and return a 
# big data frame
  driver_record = NULL # initialize
  for(idriver in 1:length(driver_id_list)){
    driver_now = driver_id_list[idriver]
    temp = rbindlist(lapply(1:200, readOneDriverOneTrip, driver_now)) # 200 trips per driver
    driver_record = rbind(driver_record, temp)
    # print progress
    left_total = length(driver_id_list)-idriver
    cat(c("Driver ", driver_now, " processed, ", left_total, " left\n"))
  }
  return(driver_record)
}

saveAllDriversToDB <- function(folder, drivers_list){
# save to SQLite database using dplyr
  if(file.exists(folder)==FALSE){
    dir.create(file.path(folder))
  }
  for(i in 1:length(drivers_list)){
    driver_now= drivers_list[i]
    dbfileName= file.path(folder, paste0(driver_now, ".db"))
    driver_db = src_sqlite(dbfileName, create=T)
    driver_df= rbindlist(lapply(1:200, readOneDriverOneTrip, driver_now)) # 200 trips per driver
    driver_sqlite = copy_to(driver_db, driver_df, name="driver_data", temporary = FALSE, 
            indexes = list("driver_id", "trip_id"))
    # print out process
    left_total = length(drivers_list)-i
    cat(c("Driver ", driver_now, " processed, ", left_total, " left\n"))
  }
  # create indexes for easy search afterwards by driver_id and trip_id
  cat('save to databse completes!\n')
  return(1)
}


# implement above functions
drivers_all = as.integer(system('ls drivers', intern=T)) # bash cmd for each driver's folder name
drivers_con = saveAllDriversToDB("drivers_dbs", drivers_all)

