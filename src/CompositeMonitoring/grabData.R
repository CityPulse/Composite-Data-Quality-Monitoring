#install.packages('rgdal')
library(rgdal)
library(RPostgreSQL)## if the package is not already installed, use install.packages('RPostgreSQL') and install.packages(DBI)  

setwd("~/svn/CorrelationMonitoring/CorrelationMonitoring")
dsn="PG:dbname='cp_traffic'"
ogrListLayers(dsn)
icons = readOGR(dsn="PG:dbname='cp_traffic'","tomtom_icons_poi_data")
icons@data[,'import_time_stamp']=as.POSIXct(icons@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps
saveRDS(icons,file = './data/tomtom_icons.RDS')

incidents = readOGR(dsn="PG:dbname='cp_traffic'","tomtom_incidents_poi_data")
incidents@data[,'import_time_stamp']=as.POSIXct(incidents@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps
saveRDS(incidents,file = './data/tomtom_incidents.RDS')

flow = readOGR(dsn="PG:dbname='cp_traffic'","tomtom_flow_segment_data")
flow@data[,'import_time_stamp']=as.POSIXct(flow@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps
saveRDS(flow,file = './data/tomtom_flow.RDS')

#min(min(flow$import_time_stamp),min(incidents$import_time_stamp),min(icons$import_time_stamp))
#create view aarhus_lines_2016_view as select * from aarhus_data natural join aarhus_lines where time_stamp >='2016-01-01'

odaa_geometries = readOGR(dsn="PG:dbname='cp_traffic'","aarhus_line")
saveRDS(odaa_geometries, file = './data/odaa_traffic_geometries.RDS')

#odaa_traffic = readOGR(dsn="PG:dbname='cp_traffic'","aarhus_lines_2016_view", disambiguateFIDs = TRUE)
#saveRDS(odaa_traffic, file = './data/odaa_traffic.RDS')

drv <- dbDriver("PostgreSQL")
if (exists('con')){
  dbDisconnect(con)
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="XXX", dbname="cp_traffic", port=5432, host="localhost")
sql <- "select report_id,vehicle_count,avg_speed, time_stamp, status, import_time_stamp from aarhus_data where time_stamp >='2016-01-01'"
odaa_traffic_data <- dbGetQuery(con,sql)
saveRDS(odaa_traffic_data, file = './data/odaa_traffic_data.RDS')



