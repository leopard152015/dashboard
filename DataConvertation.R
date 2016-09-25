# Define the list of available platforme
lookupPf <- structure(c("'MICROSOFT_BAND'", "'PHONE'", "'ANGEL_SENSOR'"),
                      .Names = c("Microsoft Band", "Mobile Phone", 
                                 "Angel Sensor"))

# Define the list of available Datasource
lookupDs <- structure(c("'ACCELEROMETER'", "'GYROSCOPE'",
                        "'HEART_RATE'","'GALVANIC_SKIN_RESPONSE'", 
                        "'RR_INTERVAL'", "'SKIN_TEMPERATURE'", 
                        "'CALORY_BURN'", "'DISTANCE'","'STEP_COUNT'",
                        "'SPEED'","'PACE'", "'MOTION_TYPE'" ), 
                      .Names = c("Accelerometer", "Gyroscope",
                                 "Heart Rate", "Galvanic Skin Response",
                                 "RR Interval", "Skin Temperature",
                                 "Calory","Distance", "Step Count", 
                                 "Speed", "Pace", "Motion Type"))

# Define the list if axis
lookupAxis <- c("X","Y","Z")

# Define the list of motion type (activity recognition)
motionType <- structure(c(0, 0, 1, 2, 3 ),
                        .Names = c("Unknown", "Idle", "Jogging",
                                   "Walking", "Running"))
  
  
convertPosixToTime <- function(dt){
  dt_stamp <- as.numeric(dt)  * 1000
  return(dt_stamp)
}

convertTimeToPosix <- function(dt_stamp){
  dt <- as.POSIXct((as.numeric(dt_stamp) + 0.1)/1000, origin = "1970-01-01")
  return(dt)
}

convertBinToDouble <- function(bin){
  tmp<-c()
  for (i in 1:length(bin)) {
    tmp[9-i] <- as.integer(as.hexmode(bin[i]))
  }  
  double <- BytesToDouble(tmp)
  double_str<-sprintf("%f",double)
  
  return(double_str)
}

convertBinToDoubleArray <- function(bin){
  double <- list()
  
  arrayBin <- as.character(unlist(bin))
  double[1] = convertBinToDouble(arrayBin[1:8])
  double[2] = convertBinToDouble(arrayBin[9:16])
  double[3] = convertBinToDouble(arrayBin[17:24])
  
  return(double)
}