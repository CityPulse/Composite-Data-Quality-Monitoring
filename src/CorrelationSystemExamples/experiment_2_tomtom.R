library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
#install.packages('RColorBrewer')
library(RColorBrewer)
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
con <- dbConnect(PostgreSQL(), user= "citypulse", password="xxxx", dbname="cp_traffic", port=5432, host="localhost")
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
pdata=points@data

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
p=list()
#for (i in 1:nrow(points@data)){
    for (i in seq(25,nrow(points@data),50)){
        t1=Sys.time()
        print(paste(i,"/",nrow(points@data)))
        sql_nearest <- paste("select report_id, st_astext(point_1_geo) p1, st_astext(point_2_geo) p2, st_distance(st_transform(point_center_geo,4326), st_geomfromtext('POINT(",points@coords[i,'coords.x1'],points@coords[i,'coords.x2'],")')::geography) as dist from aarhus_point order by dist limit ",num_sensors,";");
        queryData <- dbSendQuery(con, sql_nearest)
        data_select <- fetch(queryData, n=-1)
        next_reports <- unique(data_select[,'report_id'])
        endTime <- as.POSIXct(points@data[i,'import_time_stamp'])+daytime
        startTime = endTime-(weeks*weektime)
        p[[i]] <- queryReports_par(cl,next_reports,startTime,endTime)
        p[[i]][['icon_coord']]<-points@coords[i,]
        p[[i]][['icon_data']]<-points@data[i,]
        ex_time_par = Sys.time()-t1
        units(ex_time_par) <- "secs"
        print(paste(" ",ex_time_par,"seconds"))
        p[[i]][['ex_time_seconds']]<-ex_time_par
        p[[i]][['distances']]<-data_select
    }
parallel_stop(cl)
#saveRDS(p,file=paste0('./results/','compare_results_50th_seq_25',Sys.Date(),'.RDS'))
p  <- readRDS("/Users/danielk/experiments/compare_results_50th_2016-06-06.RDS")  
names(p[[1]][[1]])
sequence_num = seq(1,17551,50)
s=1
i=1
names(p[[sequence_num[2]]][[1]])
allspeeds <- data.frame()
for(s in 1:length(sequence_num)){
  print(s)
  event =p[[sequence_num[s]]] 
  #names(event)
  icon_data <- event[['icon_data']]
  time = as.POSIXct(as.character(icon_data$import_time_stamp))
  time
  speedssum=0
  speedlist <- list()
  for(i in 1:10){
    #names(p[[s]][[i]])
    ts <- event[[i]]$data  
    speeds = ts[(ts$time_stamp>=time-(60*15)&ts$time_stamp<=time+(60*15)),'averageSpeed_random_dct']  
    mspeeds  =min(speeds)
    #print(mspeeds)
    speedssum=speedssum+mspeeds
    speedlist[[i]] <- speeds
  }
  print(speedssum)
  allspeeds[s,'speedsum']<-speedssum/10
  #allspeeds[s,'speedlist']<-speedlist
  allspeeds[s,'severity']<-icon_data$poi_ty
  allspeeds[s,'type']<-icon_data$poi_ic
  allspeeds[s,'duration']<-icon_data$poi_dl  
  allspeeds[s,'length']<-icon_data$poi_l  
}
#allspeeds[,'minspeed']  <- min(allspeeds[,'speedlist'])

ggplot(data=allspeeds) + geom_point(aes(x=severity, y=speedsum,color=duration))

ggplot(data=allspeeds) + geom_jitter(aes(x=severity, y=speedsum,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))


corrs <- readRDS('/Users/danielk/svn/cityPulseInternal/code/NilsMA/results/joinedResults_averageSpeed.RDS')


plot(allspeeds$speedsum)
plot(p[[1]][[1]]$seasonal$vehicleCount_sum)
pdata=p[[1]]$data
ggplot(data=p[[1]]$data)+geom_line(aes(x=time_stamp,y=vehicleCount_random_dct))

