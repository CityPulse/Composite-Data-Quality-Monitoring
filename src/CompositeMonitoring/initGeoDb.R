#install.packages('RPostgreSQL')
library(RPostgreSQL)## if the package is not already installed, use install.packages('RPostgreSQL') and install.packages(DBI)  
library(jsonlite)

drv <- dbDriver("PostgreSQL")
if (exists('con')){
  dbDisconnect(con)
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="xxxx", dbname="cp_traffic", port=5435, host="localhost")
#sql <- "select report_id,vehicle_count,avg_speed, time_stamp, status, import_time_stamp from aarhus_data where time_stamp >='2016-01-01'"
#odaa_traffic_data <- dbGetQuery(con,sql)

getRouteJson <- function(con, requestId){
  print(requestId)
  sql <- paste("SELECT row_to_json(fc) 
               FROM ( SELECT 'FeatureCollection' As type, array_to_json(array_agg(f)) As features
               FROM (SELECT 'Feature' As type
               , ST_AsGeoJSON(routes.geom)::json As geometry
               , row_to_json((SELECT l FROM (SELECT request_id, route_id, cost) As l
               )) As properties
               FROM cp_routes  As routes  where request_id=",requestId," ) As f )  As fc;
               ") #and route_id=13 
  #print(sql)
  response = dbGetQuery(con,sql)
  #print(response)
  geoj <<- response$row_to_json
  #print(paste("FEATURECOUNT",response$featurecount[1,1]))
  #print(geoj)
  
  return(geoj)
}

updateRequestIds <- function(con){
  #sql <- paste("select request_id from  cp_route_requests;")
  sql <- "select request_id, st_x(from_geom) as from_x,st_y(from_geom) as from_y, st_x(to_geom) as to_x, st_y(to_geom) as to_y from  cp_route_requests;"
  request_ids_geos <<- dbGetQuery(con,sql)
  request_ids <<- request_ids_geos$request_id  
  return(request_ids)
}

