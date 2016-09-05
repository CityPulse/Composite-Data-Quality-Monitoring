library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
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
con <- dbConnect(PostgreSQL(), user= "citypulse", password="xxxx", dbname="cp_traffic", port=5435, host="localhost")
daytime=60*60*24
weektime=60*60*24*7
weeks=6
num_sensors <- 10 # to compare

#ALTER TABLE aarhus_point add column point_center_geo geometry
#UPDATE aarhus_point set point_center_geo = st_makeline(point_1_geo,point_2_geo)





#"ic": 0,  // Type: 1 = unknown, 3 = accident cleared, 6 = traffic jam, 7 = roadwork, 8 = accident, 9 = long-term roadwork, 13 = unknown
#"ty": 0,	// Severity: 0 = no delay, 1 = slow traffic, 2 = queuing traffic, 3 = stationary traffic, 4 = closed
#"r": ""		// Road name
#"l": 0,		// Length of delay in meters
#"dl": 0		// Duration-length of delay in seconds

reports <- readRDS('./metadata/reports.RDS')

#saveRDS(p,file=paste0('./results/','compare_results_50th_seq_25',Sys.Date(),'.RDS'))
#p  <- readRDS("/Users/danielk/experiments/results_try_100_2016-06-07.RDS") 
#p  <- readRDS("/Users/danielk/experiments/results_try_all_2016-06-08.RDS") 
p  <- readRDS("/Users/danielk/experiments/results_try_all_withcount_2016-06-26.RDS") 

names(p[[1]])

result.df <-data.frame(matrix(unlist(
  lapply(p, function(x) {
    units(x$ex_time_seconds)<-"secs"
    c(
      x$severity,
      x$type,
      x$duration,
      x$length,
      mean(sort(unlist(lapply(x$speedlist, function(x) min(x))))[1:5]),
      sort(unlist(lapply(x$speedlist, function(y) min(y))))[1],
      mean(unlist(lapply(x$speedlist, function(y) min(y)))),
      mean(unlist(lapply(x$speedlist, function(y) mean(y[3:4])))),
      mean(sort(unlist(lapply(x$countlist, function(x) min(x))))[1:5]),
      sort(unlist(lapply(x$countlist, function(y) min(y))))[1],
      mean(unlist(lapply(x$countlist, function(y) min(y)))),
      mean(unlist(lapply(x$countlist, function(y) mean(y[3:4])))),
      x$ex_time_seconds
    )
  })
  ), nrow=length(p), byrow=T),stringsAsFactors=FALSE)
colnames(result.df) <- c('severity','type','duration','length','speed_min5','speed_min1','speed_mean','speed_ts','count_min5','count_min1','count_mean','count_ts','execution_time_s')

result.df[result.df$severity==0,'severity_text']<-'0 Unknown'
result.df[result.df$severity==1,'severity_text']<-'1 Slow Traffic'
result.df[result.df$severity==2,'severity_text']<-'2 Queuing Traffic'
result.df[result.df$severity==3,'severity_text']<-'3 Stationary Traffic'
result.df[result.df$severity==4,'severity_text']<-'4 Closed'

g_min5 <- ggplot(data=result.df) + geom_jitter(aes(x=severity_text, y=speed_min5,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min5
ggsave(g_min5, filename='./results/plots/speed_impact_on_events_min_5.pdf', units="cm", width=30,height=19)

g_min_box5 <- ggplot(data=result.df) + geom_boxplot(aes(x=severity_text, y=speed_min5,color=length))+ #+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
  labs(x = "Incident: Severity Level")+
  labs(y = "Flow: Max. Speed Change (Random Component)")
g_min_box5
ggsave(g_min_box5, filename='./results/plots/speed_impact_on_events_min_5_boxplot.pdf', units="cm", width=15,height=10)

g_min1 <- ggplot(data=result.df) + geom_jitter(aes(x=severity_text, y=speed_min1,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min1
ggsave(g_min1, filename='./results/plots/speed_impact_on_events_min_1.pdf', units="cm", width=30,height=19)

g_min_box1 <- ggplot(data=result.df) + geom_boxplot(aes(x=severity_text, y=speed_min1,color=length))+theme_bw()+
  labs(x = "Incident Severity Level (TomTom)")+
  labs(y = "Max. Average Speed Change (Irregular Component, ODAA)")
g_min_box1

ggsave(g_min_box1, filename='./results/plots/Paper_speed_impact_on_events_min_1_boxplot.pdf', units="cm", width=14,height=8)

#double plot

result.df2 <- result.df[,c('severity_text','speed_min1','count_min1')]
colnames(result.df2) <- c('severity_text','Avg. Speed (Irregular Comp.)','VehicleCount (Raw Data)')
result.melt <- melt(result.df2, id='severity_text')
g_min_boxm <- ggplot(data=result.melt) + geom_boxplot(aes(x=severity_text, y=value))+
  theme_bw()+
  labs(x = "Incident Severity Level (TomTom)")+
  #labs(y = "Max. Average Speed Change (Irregular Component, ODAA)")+
  labs(y = "Max. Value Change (ODAA)")+
  facet_grid(variable ~ . , scales = "free") +
  theme(legend.position = "none",strip.text.x=element_text(angle=-90))
g_min_boxm
ggsave(g_min_boxm, filename='./results/plots/Paper_speed_and_vc_impact_on_events_min_1_boxplot.pdf', units="cm", width=16,height=14)

unique(result.melt$severity_text)

#ggplot(data=result.df) + geom_jitter(aes(x=severity_text, y=speed_mean,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
#ggplot(data=result.df) + geom_jitter(aes(x=severity_text, y=speed_ts,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))


#performance:
#ggplot(data=result.df) + geom_jitter(aes(x=severity_text, y=execution_time_s,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
ggplot(data=result.df) + geom_boxplot(aes(x=severity_text, y=execution_time_s,color=length))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_cdf <- ggplot(data=result.df, aes(x=execution_time_s)) + stat_ecdf(geom = "step") + 
          labs(title = "Cor. Monitoring CDF (6weeks, 10 sensors, time series decompoition, comparison)")+
          labs(x = "Execution Time (s)")+
          labs(y = "Probability")
g_cdf
ggsave(g_cdf, filename='./results/plots/CorMonCDF_1.pdf', units="cm", width=30,height=19)

pairs(result.df[,1:8])

