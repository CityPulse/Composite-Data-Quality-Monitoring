library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
library(rgeos)
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


# alter table tomtom_flow_segment_data add column confidence100 integer;
# update  tomtom_flow_segment_data set confidence100 = 100*confidence;

library(rgdal)
dsn="PG:dbname='cp_traffic'"
ogrListLayers(dsn)
ogrInfo(dsn, "tomtom_flow_segment_data")
        
flow = readOGR(dsn="PG:dbname='cp_traffic'",layer="tomtom_flow_segment_data", verbose = TRUE,stringsAsFactors=FALSE)
flow@data[,'import_time_stamp']=as.POSIXct(flow@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps
flow@data[,'confidence']=as.numeric(flow@data[,'confidence100'])/100

flow5 <- flow[(flow@data$current_speed-flow@data$free_flow_speed)<(-50),]

reports <- readRDS('./metadata/reports.RDS')
offsets <- readRDS('./metadata/offset.RDS')
nrow(flow5)
cl <- parallel_prepare(no_cores)
all_p=list()
allspeeds <- data.frame()

for (i in 1:nrow(flow5)){
    wktline <- writeWKT(flow5[i,],byid=TRUE)
    p=list()
    t1=Sys.time()
    print(paste(i,"/",nrow(flow5)))
    sql_nearest <- paste("select report_id, st_astext(point_1_geo) p1, st_astext(point_2_geo) p2, st_distance(st_transform(point_center_geo,4326), st_geomfromtext('",wktline,"')::geography) as dist from aarhus_point order by dist limit ",num_sensors,";");
    queryData <- dbSendQuery(con, sql_nearest)
    data_select <- fetch(queryData, n=-1)
    next_reports <- unique(data_select[,'report_id'])
    endTime <- as.POSIXct(flow5@data[i,'import_time_stamp'])+daytime
    startTime = endTime-(weeks*weektime)
    timeseries <- queryReports_par(cl,next_reports,startTime,endTime)
    p[['flow_geom']]<-flow5@lines[i]
    p[['flow_data']]<-flow5@data[i,]
    p[['distances']]<-data_select
    time = as.POSIXct(as.character(p[['flow_data']]$import_time_stamp))
    speedssum=0
    speedlist <- list()
    p[['timeseries']]<- timeseries
    for(j in 1:num_sensors){
      ts <- timeseries[[j]]$data
      speeds = ts[(ts$time_stamp>=time-(60*15)&ts$time_stamp<=time+(60*15)),'averageSpeed_random_dct']  
      mspeeds  =min(speeds)
      #print(mspeeds)
      speedssum=speedssum+mspeeds
      speedlist[[j]] <- speeds
    }
    print(speedssum)
    p[['speedsum']]<-speedssum/10
    p[['speedlist']]<-speedlist
    p[['current_speed']]<-p[['flow_data']]$current_speed
    p[['free_flow_speed']]<-p[['flow_data']]$free_flow_speed
    p[['current_travel_time']]<-p[['flow_data']]$current_travel_time  
    p[['free_flow_travel_time']]<-p[['flow_data']]$free_flow_travel_time  
    p[['confidence']]<-p[['flow_data']]$confidence
    
    
    ex_time_par = Sys.time()-t1
    units(ex_time_par) <- "secs"
    print(paste(" ",ex_time_par,"seconds"))
    p[['ex_time_seconds']]<-ex_time_par
    all_p[[i]] <- p
  }
  parallel_stop(cl)
  
  all_speedsums<-unlist(lapply(all_p,function(x) x$speedsum))
  which(all_speedsums<(-12))
  
  for(i in which(all_speedsums<(-12))){
    p1 <- all_p[[i]]
    pts <- p1$timeseries
    for (j in 1:length(pts)){
      pts1 <- pts[[j]]
      #i=54#j=6
      #plot(pts1$seasonal$vehicleCount)
      
      # dfp <- data.frame(pts1$seasonal$vehicleCount)
      # dd <- data.frame(pts1$data$time_stamp)
      # dd[,'raw'] <- data.frame(pts1$data$vehicleCount_raw)
      # dd[,'random'] <- data.frame(pts1$data$vehicleCount_random)
      # dd[,'random_dct'] <- data.frame(pts1$data$vehicleCount_random_dct)
      dfp <- data.frame(pts1$seasonal$averageSpeed)
      dd <- data.frame(pts1$data$time_stamp)
      dd[,'raw'] <- data.frame(pts1$data$averageSpeed_raw)
      dd[,'random'] <- data.frame(pts1$data$averageSpeed_random)
      dd[,'random_dct'] <- data.frame(pts1$data$averageSpeed_random_dct)
      
      selected_rows <- nrow(dd)-nrow(dfp):nrow(dd)
      dd_week <- dd[(nrow(dd)-nrow(dfp)+1):(nrow(dd)),]
      dd_week[,'seasonal'] <- dfp[,1]
      colnames(dd_week) <- c('Date','Raw','Irregular','Irregular_DCT','Seasonal')
      dd_week = dd_week[,c('Date','Seasonal','Raw','Irregular','Irregular_DCT')]
      meltdf = melt(dd_week,id='Date')
      #meltdf <- meltdf_full[nrow(meltdf)-nrow(dfp):nrow(meltdf),]
      
     event_time <- as.POSIXct(p1$flow_data$import_time_stamp-130000)
     free_flow = p1$flow_data$free_flow_speed
     current_flow = p1$flow_data$current_speed
     delta_flow =current_flow-free_flow
    decomp_plot <- ggplot(meltdf,aes(x=Date,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
      theme_bw() +
      ylab("Average Vehicle Speed (km/h)") + xlab("Date") + 
      ggtitle(paste("ODAA TS Decomposition -> TomTom FreeFlow(km/h):",free_flow,"CurrentFlow(km/h)",current_flow))+
      facet_grid(variable ~ . , scales = "free") +
      theme(legend.position = "none")+
      geom_vline(aes(xintercept=as.numeric(event_time)), linetype=2, colour="black")
      #geom_text(aes(x=event_time, label=paste('Flow:',delta_flow,"(km/h)"), y=0), colour="blue", angle=90, text=element_text(size=8)) 
      #geom_text(aes(x=event_time, label='\nFlow', y=0), colour="blue", angle=90, text=element_text(size=8)) 
      decomp_plot
      
      #ggsave(decomp_plot, file=paste0('results/Flow_AverageSpeedDecomposition_lrg.pdf'),width = 8,height = 8)
      
      ggsave(decomp_plot, file=paste0('results/Flow_AverageSpeedDecomposition_',i,'_',j,'.pdf'),width = 12,height = 12)
    }
  }   
  #annotate("rect", xmin=1995-01-01,xmax=1996-01-01,ymin= 10, ymax=300, alpha = .2,fill="blue")
  
  
  #saveRDS(all_p,file=paste0('./results/','results_flow_try_all_more5_',Sys.Date(),'.RDS'))
  
  summary(all_p)
  for (i in all_p){
    i=1
    g <- SpatialLines(all_p[[i]]$flow_geom)
    wkt <- writeWKT(g)
    sql_nearest = paste0("select id from ways_vertices_pgr order by st_distance(the_geom, st_setsrid(ST_GeomFromText('",wkt,"'), 4326)) limit 1");
    queryData <- dbSendQuery(con, sql_nearest)
    start_nodeID <- fetch(queryData, n=-1)
    
    distances <- all_p[[i]]$distances
    for (d in 1:nrow(distances)){
      wkt = paste0("st_makeline(ST_GeomFromText('",distances[d,'p1'],"'),ST_GeomFromText('",distances[d,'p2'],"'))")
      wkt
      sql_nearest = paste0("select id from ways_vertices_pgr order by st_distance(the_geom, st_setsrid(",wkt,",4326)) limit 1");
      sql_nearest
      queryData <- dbSendQuery(con, sql_nearest)
      end_nodeID <- fetch(queryData, n=-1)
      print(nodeID)
    }

    sql = paste0("SELECT  st_astext(ST_LineMerge(st_union(st_simplify(the_geom,100)))) as geometry, ",
                 "st_length(ST_LineMerge(st_union(the_geom)),true)::int as length_m, ",
                 "sum(st_length(the_geom,true) / (maxspeed_forward /3.6)*0.75)::int as time_s, ",
                 "sum(route.cost) as total_cost, ",
                 "count(osm_id)::int as distinct_osm_ids, ",
                 "json_agg(edge_id)::text as edges, ",
                 "json_agg(cityCostMulti)::text as city_cost_multi ",
                 "FROM ways JOIN (SELECT seq, node AS node, edge AS edge_id, cost ",
                 "FROM pgr_dijkstra('SELECT gid AS id, source::bigint, target::bigint, ",
                 "(length)::double precision AS cost FROM ways as w '",
                 ",",sql_nearest,",",end_nodeID,", false)   ) AS route ON ways.gid = route.edge_id")
    
    
  sql_route
  queryData_route <- dbSendQuery(con, sql_route)
  end_nodeID_route <- fetch(queryData, n=-1)
  
  }
