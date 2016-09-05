library(plyr)
library(ggplot2)
library(data.table)
library(parallel)

library(RPostgreSQL)

setwd("~/svn/CorrelationMonitoring/CorrelationSystem")
source("util.R")
source("composite_m_lib.R")

weektime=60*60*24*7

# Calculate the number of cores
no_cores <- detectCores() - 1


# Minimum 5 minute steps
rate = 5*60
dctFilter = 21

idColumn = "report_id"
timestampColumn = "time_stamp"
dataColumns = c("vehicle_count", "avg_speed")

identifier = c("vehicleCount", "averageSpeed")
INPUTTYPES = c("raw", "dct", "random", "random_dct")

all_reports <- readRDS("metadata/reports.RDS")
reports <- sort(all_reports[,1])
total <- length(reports)
tempBaseDir = "/tmp/save"

idColumn = "report_id"
timestampColumn = "time_stamp"
dataColumns = c("vehicle_count", "avg_speed")



#Experiment on Scalability..
cl <- parallel_prepare(no_cores)
times = data.frame()

weeks=c(6,12,24)
#sensor_steps = seq(1, length(reports),50)
#sensor_steps =1:length(reports)
#sensor_steps = c(1,2,3,4,5,10,20,40,80,160,320,449)
sensor_steps =1:50
df_row=1
for(run in 1:10){
  for(w in 1:length(weeks)){
    startTime = Sys.time()-(weeks[w]*weektime)
    
    for( i in sensor_steps){
      #i=1
      #w=1
      #run=1
      repids <-sample(1:length(reports), i)
      print(paste("starParallel",weeks[w],"weeks, ",i,"sensors on",no_cores,"cores"))
      print(paste(reports[repids], startTime))
      t2=Sys.time()
      p <- queryReports_par(cl,reports[repids],startTime)
      ex_time_par = Sys.time()-t2
      units(ex_time_par)<-"secs"
      print(paste(ex_time_par,"secs"))
      
      times[df_row,1]=i
      times[df_row,2]=ex_time_par
      times[df_row,3]=weeks[w]
      times[df_row,4]=run
      df_row = df_row+1
    }
  }
}
parallel_stop(cl)
colnames(times) <- c('sensors', 'seconds', 'weeks', 'run')
saveRDS(times,file = paste0('./results/','scalability_test_6_',Sys.Date(),'.RDS'))


