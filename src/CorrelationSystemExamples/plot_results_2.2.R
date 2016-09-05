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
con <- dbConnect(PostgreSQL(), user= "citypulse", password="qPoRD3Zk", dbname="cp_traffic", port=5435, host="localhost")
daytime=60*60*24
weektime=60*60*24*7
weeks=6
num_sensors <- 10 # to compare

#ALTER TABLE aarhus_point add column point_center_geo geometry
#UPDATE aarhus_point set point_center_geo = st_makeline(point_1_geo,point_2_geo)



reports <- readRDS('./metadata/reports.RDS')

#with full datalist
p  <- readRDS("/Users/danielk/experiments/results_try_all_with_all_data_2016-06-27.RDS") 

# names(p[[1]])
# p[[1]]$datalist[[1]]$vehicleCount_raw
# min(p[[1]]$distances$dist)
# for(i in 1:length(p)){
#   x=ü
# }

# 
# result.3d <- lapply(p, function(x) {
#   list(
#   length(x$icon_coord),
#   x$severity,
#   x$distances,
#   x$speedlist)
# })

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
      mean(unlist(lapply(x$datalist, function(y) min(y$vehicleCount_raw)))),
      min(unlist(lapply(x$datalist, function(y) min(y$vehicleCount_raw)))),
      min(x$datalist[[1]]$vehicleCount_raw),
      mean(unlist(lapply(x$datalist, function(y) mean(y$vehicleCount_random)))),
      mean(x$datalist[[1]]$vehicleCount_random),
      min(x$distances$dist),
      x$ex_time_seconds
    )
  })
  ), nrow=length(p), byrow=T),stringsAsFactors=FALSE)
colnames(result.df) <- c('severity','type','duration','length','speed_min5','speed_min1','speed_mean','speed_ts','count_min5','count_min1','count_mean','count_ts','count_raw','min_count_raw','mindist_count_raw','mean_count','mean_count2','min_dist', 'execution_time_s')
#countlist =list()
#for(i in 1:length(p)) p[[1]]$datalist[[1]]$vehicleCount_raw



result.df[result.df$severity==0,'severity_text']<-'0 Unknown'
result.df[result.df$severity==1,'severity_text']<-'1 Slow Traffic'
result.df[result.df$severity==2,'severity_text']<-'2 Queuing Traffic'
result.df[result.df$severity==3,'severity_text']<-'3 Stationary Traffic'
result.df[result.df$severity==4,'severity_text']<-'4 Closed'
summary(result.df)
# IMPORTANTREDUCTION to suit euclidean pattern
result.df <- result.df[result.df$min_dist<50,]

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
#ggsave(g_min1, filename='./results/plots/speed_impact_on_events_min_1.pdf', units="cm", width=30,height=19)

g_min_box1 <- ggplot(data=result.df) + geom_boxplot(aes(x=severity_text, y=speed_min1,color=length))+theme_bw()+
  labs(x = "Incident Severity Level (TomTom)")+
  labs(y = "Max. Average Speed Change (Irregular Component, ODAA)")
g_min_box1

#ggsave(g_min_box1, filename='./results/plots/Paper_speed_impact_on_events_min_1_boxplot.pdf', units="cm", width=14,height=8)

#double plot
names(result.df)
result.df2 <- result.df[,c('severity_text','speed_min1','count_min1')]
colnames(result.df2) <- c('severity_text','Avg. Speed (Irregular Comp.)','VehicleCount (Irregular Component)')
result.melt <- melt(result.df2, id='severity_text')
g_min_boxm <- ggplot(data=result.melt) + geom_boxplot(aes(x=severity_text, y=value))+
  theme_bw()+
  labs(x = "Incident Severity Level (TomTom)")+
  #labs(y = "Max. Average Speed Change (Irregular Component, ODAA)")+
  labs(y = "Max. Value Change (ODAA)")+
  facet_grid(variable ~ . , scales = "free") +
  theme(legend.position = "none",strip.text.x=element_text(angle=-90))
g_min_boxm
ggsave(g_min_boxm, filename='./results/plots/Paper_speed_and_vc_impact_on_events_min_1_boxplot_V2.pdf', units="cm", width=16,height=14)


#cdf
names(result.df)
result.df2 <- result.df[,c('severity_text','speed_min1','count_min1')]
#result.df2 <- result.df[,c('severity_text','speed_min1','mean_count2')]
colnames(result.df2) <- c('severity_text','Avg. Speed','VehicleCount')

mean_count

result.melt <- melt(result.df2, id='severity_text')
g_min_cdf <- ggplot(data=result.melt, aes(x=value)) + stat_ecdf(geom = "step")+ #geom_boxplot(aes(x=severity_text, y=value))+
  theme_bw()+
  #facet_grid(variable ~ severity_text)+#,  scales = "free")+
  facet_grid( severity_text ~ variable,  scales = "free")+
  labs(x = "(KM/H)     - Value of Irregular Component -     (Number of Cars)")+
  labs(y = "Cummulated Probability (CDF)")+
  #facet_grid(variable ~ . , scales = "free") +
  theme(legend.position = "none")
g_min_cdf
ggsave(g_min_cdf, filename='./results/plots/Paper_speed_and_vc_impact_on_events_min_1_CDF_v4.pdf', units="cm", width=16,height=16)

#ggsave(g_min_boxm, filename='./results/plots/Paper_speed_and_vc_impact_on_events_min_1_CDF_v4.png')






##just nearby ones...

names(result.df)
result.df3 <- result.df[,c('severity_text','count_raw')]
#colnames(result.df3) <- c('severity_text','VehicleCount (Raw Data)')
'mindist_count_raw','min_count_raw','count_raw'
result.df3 <- result.df[,c('severity_text','mindist_count_raw')]


g_cdf_vc <- ggplot(data=result.df3, aes(x=mindist_count_raw)) + stat_ecdf(geom = "step") + 
  facet_grid(severity_text ~ . , scales = "free")+
  theme_bw()+
    labs(x = "VehicleCount (Raw Data, ODAA)")+
    labs(y = "Probability")#+
    scale_x_sqrt()
    #xlim(c(0,5))
g_cdf_vc
ggsave(g_cdf_vc, filename='./results/plots/Paper_vc_CDF_sqrt.pdf', units="cm", width=14,height=11)


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

