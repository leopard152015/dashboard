library(RSQLite)
source("DataConvertation.R")

#dbFields <- structure(list(ds_id = c(),
#                           ds_name = c(),
#                      platform_type = c()
#),
#.Names = c("ds_id","platform_type"), 
#class = "data.frame"
#)



selectDatasourceId <- function(connection, source_name, platform_type){
  sql_selection <- paste("select ds_id from datasources where datasource_type =",source_name)
  sql_selection <- paste(sql_selection, "and platform_type = ", platform_type)
 # print(sql_selection)
  result <- dbGetQuery(connection, sql_selection)


 
  fields <- data.frame(result[1],source_name, platform_type) #result[2])
  
  return(fields)

 # dbClearResult(result)
}

select1AxisesData <- function(connection, id){
  sql_selection = paste("select CAST(datetime AS character), sample from rawdata where datasource_id=",id)
  tmp <- dbGetQuery(connection, sql_selection)
  len = nrow(tmp)
  
  dt_array <-  as.character(unlist(tmp[1]))
  d_array <- (tmp[2])
  dt <- convertTimeToPosix(dt_array) #mapply(convertTimeToPosix, dt_array)

  coordinate <- function(x){
    as.numeric(convertBinToDouble( as.character( unlist(d_array[x, ]))))
  }
  n <- c(1:len)
  
  v.coordinate <- Vectorize(coordinate,vectorize.args='x')(n)
  coord <- v.coordinate
  
  x <- coord

  counter <- c(1:len)
  
  devData <- data.frame(dt, counter, x)


}

select3AxisesData <- function(connection, id){
  sql_selection = paste("select CAST(datetime AS character), sample from rawdata where datasource_id=",id)
  tmp <- dbGetQuery(connection, sql_selection)
  len = nrow(tmp)
  
  dt_array <-  as.character(unlist(tmp[1]))
  d_array <- (tmp[2])
  
  dt <- convertTimeToPosix(dt_array)#mapply(convertTimeToPosix, dt_array)

   coordinate <- function(x){
    as.numeric(convertBinToDoubleArray( as.character( unlist(d_array[x, ]))))
  }
  
  n <- c(1:len)
  
  v.coordinate <- Vectorize(coordinate,vectorize.args='x')(n)
  coord <- v.coordinate
  
  x <- coord[1,]
  y <- coord[2,]
  z <- coord[3,]
  counter <- c(1:len)
  

  devData <- data.frame(dt, counter, x, y, z)# ldply(output, data.frame) #rbind(accelerometer, as.data.frame( output))

  
  return(devData)
}

selectMovementData <- function(connection, datasource, platform_type){
  
  dbFields <- selectDatasourceId(connection, datasource, platform_type)
  devData <- select3AxisesData(connection, dbFields$ds_id)
  
  return(devData)#[1:1500,])
}

selectHealthData <- function(connection, datasource, platform_type ){
  dbFields <- selectDatasourceId(connection, datasource, platform_type)
  devData <- select1AxisesData(connection, dbFields$ds_id)
  
  return(devData)#[1:1500,])
}
openDatabase <- function(database_name){
  sqlite.driver <- dbDriver("SQLite")

  connection <- dbConnect(sqlite.driver,
                       dbname = database_name)

  return(connection)
}