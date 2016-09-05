library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)

setwd("~/svn/CorrelationMonitoring/CorrelationSystem")
source("util.R")
source("composite_m_lib.R")

no_cores <- detectCores() - 1
idColumn = "report_id"
timestampColumn = "time_stamp"
dataColumns = c("vehicle_count", "avg_speed")
identifier = c("vehicleCount", "averageSpeed")
INPUTTYPES = c("raw", "dct", "random", "random_dct")
rate = 5*60
dctFilter = 21



if (exists('con')){
  dbDisconnect(con)
  #dbClearResult(dbListResults(con)[[1]])
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="qPoRD3Zk", dbname="cp_traffic", port=5432, host="localhost")
daytime=60*60*24
weektime=60*60*24*7
weeks=6
num_sensors <- 10 # to compare

#ALTER TABLE aarhus_point add column point_center_geo geometry
#UPDATE aarhus_point set point_center_geo = st_makeline(point_1_geo,point_2_geo)



library(rgdal)
dsn="PG:dbname='cp_traffic'"
ogrListLayers(dsn)
points = readOGR(dsn="PG:dbname='cp_traffic'","tomtom_icons_poi_data")
points@data[,'import_time_stamp']=as.POSIXct(points@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps

#"ic": 0,	// Type: 1 = unknown, 3 = accident cleared, 6 = traffic jam, 7 = roadwork, 8 = accident, 9 = long-term roadwork, 13 = unknown
#"ty": 0,	// Severity: 0 = no delay, 1 = slow traffic, 2 = queuing traffic, 3 = stationary traffic, 4 = closed
#"r": ""		// Road name
#"l": 0,		// Length of delay in meters
#"dl": 0		// Duration-length of delay in seconds

reports <- readRDS('./metadata/reports.RDS')
#offset <- readRDS('./metadata/offset.RDS')
# icons_df = data.frame(points@coords[,'coords.x1'],points@coords[,'coords.x2'],
#                         as.POSIXct(points@data[,'import_time_stamp']),
#                         points@data[,'poi_ic'],
#                         points@data[,'poi_ty'],
#                         points@data[,'poi_l'],
#                         points@data[,'poi_dl']
#                 )
#colnames(icons_df) <- c('x','y','timestamp','ic','ty','l','dl')

cl <- parallel_prepare(no_cores)
all_p=list()
allspeeds <- data.frame()
for (i in 1:nrow(points@data)){
#for (i in 1:1000){
  p=list()
  t1=Sys.time()
  print(paste(i,"/",nrow(points@data)))
  sql_nearest <- paste("select report_id, st_astext(point_1_geo) p1, st_astext(point_2_geo) p2, st_distance(st_transform(point_center_geo,4326), st_geomfromtext('POINT(",points@coords[i,'coords.x1'],points@coords[i,'coords.x2'],")')::geography) as dist from aarhus_point order by dist limit ",num_sensors,";");
  queryData <- dbSendQuery(con, sql_nearest)
  data_select <- fetch(queryData, n=-1)
  next_reports <- unique(data_select[,'report_id'])
  endTime <- as.POSIXct(points@data[i,'import_time_stamp'])+daytime
  startTime = endTime-(weeks*weektime)
  timeseries <- queryReports_par(cl,next_reports,startTime,endTime)
  p[['icon_coord']]<-points@coords[i,]
  p[['icon_data']]<-points@data[i,]

  p[['distances']]<-data_select
  time = as.POSIXct(as.character(p[['icon_data']]$import_time_stamp))
  speedssum=0
  speedlist <- list()
  countsum=0
  countlist <- list()
  datalist <- list()
  for(j in 1:10){
    ts <- timeseries[[j]]$data
    speeds = ts[(ts$time_stamp>=time-(60*15)&ts$time_stamp<=time+(60*15)),'averageSpeed_random_dct']  
    counts = ts[(ts$time_stamp>=time-(60*15)&ts$time_stamp<=time+(60*15)),'vehicleCount_random_dct']  
    datasets = ts[(ts$time_stamp>=time-(60*15)&ts$time_stamp<=time+(60*15)),]
    mspeeds  =min(speeds)
    mcounts = min(counts)
    #print(mspeeds)
    speedssum=speedssum+mspeeds
    speedlist[[j]] <- speeds
    countsum=countsum+mcounts
    countlist[[j]] <- counts
    datalist[[j]] <- datasets
  }
  p[['speedsum']]<-speedssum/10
  p[['speedlist']]<-speedlist
  p[['countsum']]<-countsum/10
  p[['countlist']]<-countlist
  p[['datalist']]<-datalist
  p[['severity']]<-p[['icon_data']]$poi_ty
  p[['type']]<-p[['icon_data']]$poi_ic
  p[['duration']]<-p[['icon_data']]$poi_dl  
  p[['length']]<-p[['icon_data']]$poi_l  
  
  ex_time_par = Sys.time()-t1
  units(ex_time_par) <- "secs"
  print(paste(" ",ex_time_par,"seconds"))
  p[['ex_time_seconds']]<-ex_time_par
  all_p[[i]] <- p
}
saveRDS(all_p,file=paste0('./results/','results_try_all_with_all_data_',Sys.Date(),'.RDS'))


parallel_stop(cl)