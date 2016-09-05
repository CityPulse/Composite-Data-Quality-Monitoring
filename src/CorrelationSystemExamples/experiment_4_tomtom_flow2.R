
library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
library(rgeos)
#setwd("~/svn/CorrelationMonitoring/CorrelationSystem")
setwd("~/svn/CityPulseDevelopment/trunk/A4.3/CorrelationMonitoring/CorrelationSystem")

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
num_sensors <- 5 # to compare

#ALTER TABLE aarhus_point add column point_center_geo geometry
#UPDATE aarhus_point set point_center_geo = st_makeline(point_1_geo,point_2_geo)



library(rgdal)
dsn="PG:dbname='cp_traffic'"
ogrListLayers(dsn)
ogrInfo(dsn, "tomtom_flow_segment_data")
        
flow = readOGR(dsn="PG:dbname='cp_traffic'",layer="tomtom_flow_segment_data", verbose = TRUE,stringsAsFactors=FALSE)
flow@data[,'import_time_stamp']=as.POSIXct(flow@data[,'import_time_stamp']) ##incridible memory leak when saving single factorised timestamps
flow5 <- flow[(flow@data$current_speed-flow@data$free_flow_speed)<(-5),]

set.seed(1)             # for reproducible example
setwd(" <directory with your shapefiles> ")
spl <- readOGR(dsn=".", "Strasse", encoding="latin-9")
spl <- spl[spl$SHAPE_LEN %in% tail(sort(spl$SHAPE_LEN),100),]
shp       <- spTransform(spl, CRS("+proj=longlat +datum=WGS84"))
shp.df    <- data.frame(id=rownames(shp@data),
                        values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)

data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")

ggmap(get_map(unlist(geocode("Cologne")),zoom=11))+
  geom_path(data=data_merged,size=1,
            aes(x=long,y=lat,group=group,color=factor(values)))+
  labs(x="",y="")+
  theme(axis.text=element_blank(),axis.ticks=element_blank())








reports <- readRDS('./metadata/reports.RDS')
#offset <- readRDS('./metadata/offset.RDS')
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
    
    itss = flow5@data[,'import_time_stamp']
    its=flow5@data[1, "import_time_stamp"]
    as.POSIXct(its)
    c("coord_id",   "import_time_stamp", "style", "frc", "current_speed", "free_flow_speed", "current_travel_time", "free_flow_travel_time", "confidence")
    
    p[['distances']]<-data_select
    time = as.POSIXct(as.character(p[['flow_data']]$import_time_stamp))
    speedssum=0
    speedlist <- list()
    for(j in 1:10){
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
  
  
  saveRDS(all_p,file=paste0('./results/','results_flow_try_all_more5_',Sys.Date(),'.RDS'))
  
  
  ex_time_par = Sys.time()-t1
  units(ex_time_par) <- "secs"
  print(paste(" ",ex_time_par,"seconds"))
  p[['ex_time_seconds']]<-ex_time_par
  all_p[[i]] <- p
}
parallel_stop(cl)


#saveRDS(all_p,file=paste0('./results/','results_flow_try_1000_',Sys.Date(),'.RDS'))
all_p <- readRDS(file=paste0('./results/','results_flow_try_1000_',Sys.Date(),'.RDS'))
p1 <- all_p[[1]]
result.df <-data.frame(matrix(unlist(
  lapply(all_p, function(x) {
    units(x$ex_time_seconds)<-"secs"
    c(
      x$current_speed,
      x$free_flow_speed,
      x$current_speed-x$free_flow_speed,
      x$current_travel_time,
      x$free_flow_travel_time,
      x$current_travel_time- x$free_flow_travel_time,
      x$confidence,      
      mean(sort(unlist(lapply(x$speedlist, function(x) min(x))))[1:5]),
      sort(unlist(lapply(x$speedlist, function(y) min(y))))[1],
      mean(unlist(lapply(x$speedlist, function(y) min(y)))),
      mean(unlist(lapply(x$speedlist, function(y) mean(y[3:4])))),
      x$ex_time_seconds,
      x$flow_data$import_time_stamp,
      mean(x$flow_geom[[1]]@Lines[[1]]@coords[,1]),
      mean(x$flow_geom[[1]]@Lines[[1]]@coords[,2])
    )
  })
), nrow=length(all_p), byrow=T),stringsAsFactors=FALSE)
colnames(result.df) <- c('current_speed','free_flow_speed','speed_delta','current_travel_time','free_flow_travel_time','time_delta','confidence','speed_min5','speed_min1','speed_mean','speed_ts','execution_time_s','import_time_stamp','x','y')

g_min5 <- ggplot(data=result.df) + geom_jitter(aes(x=speed_delta, y=speed_min5,color=confidence))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min5
#ggsave(g_min5, filename='./results/plots/flow_speed_impact_on_events_min_5.pdf', units="cm", width=30,height=19)
g_min1 <- ggplot(data=result.df) + geom_jitter(aes(x=speed_delta, y=speed_min1,color=confidence))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min1
#ggsave(g_min1, filename='./results/plots/flow_speed_impact_on_events_min_1.pdf', units="cm", width=30,height=19)


result_geo <-data.frame(matrix(unlist(
  lapply(all_p, function(x) {
    units(x$ex_time_seconds)<-"secs"
    c(
     x$flow_geom[[1]]
    )
  })
), nrow=length(all_p), byrow=T),stringsAsFactors=FALSE)
r1 <- unlist(result_geo[,1])
spl <- SpatialLines(r1)
spl_unique <- SpatialLines(unique(r1))
plot(spl)


library(scatterplot3d)
with(result.df, {
  scatterplot3d(speed_delta,   # x axis
                time_delta,     # y axis
                import_time_stamp,    # z axis
                main="3-D Scatterplot Example 1")
})


library(scatterplot3d)
# create column indicating point color
result.df$pcolor <- "blue"
result.df$pcolor[result.df$speed_delta<= -10] <- "darkgreen"
result.df$pcolor[result.df$speed_delta<= -20] <- "green"
result.df$pcolor[result.df$speed_delta<= -30] <- "yellow"
result.df$pcolor[result.df$speed_delta<= -40] <- "orange"
result.df$pcolor[result.df$speed_delta<= -50] <- "red"
summary(result.df)

with(result.df, {
  s3d <- scatterplot3d(x,
                       y,      # x y and z axis
                       import_time_stamp,
                       color=pcolor, pch=19,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=1,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot Example 4",
                       xlab="x",
                       ylab="y",
                       zlab="z")
#  s3d.coords <- s3d$xyz.convert(x, y, import_time_stamp)
#  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
#       labels=row.names(result.df),       # text to plot
#       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
#  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Speed Delta",
         c("<10","<= -10", "<= -20", "<= -30", "<= -40", "<= -50"), fill=c("blue","darkgreen","green","yellow","orange","red"))
})


