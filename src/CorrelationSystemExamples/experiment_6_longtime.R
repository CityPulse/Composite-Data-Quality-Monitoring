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
tableName="aarhus_data"
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
sqlQuery <- paste("select max(",timestampColumn," ) from ",tableName," ;")

sqlQuery <- paste0("SELECT ", paste0(c(idColumn, timestampColumn, dataColumns), collapse=','), " FROM aarhus_data ORDER BY ", timestampColumn);
sqlQuery <- paste("select ",idColumn,",avg(",dataColumns[1],"),date_trunc( 'month', ",timestampColumn," ) from ",tableName," group by ",idColumn,",date_trunc( 'month', time_stamp );")
sqlQuery <- paste("select ",idColumn,",avg(",dataColumns[1],"),date_trunc( 'week', ",timestampColumn," ) from ",tableName," group by ",idColumn,",date_trunc( 'week', time_stamp );")
sqlQuery <- paste("select ",idColumn,",",dataColumns[1],",",timestampColumn,"  from ",tableName)

print(sqlQuery)
queryData <- dbSendQuery(con, sqlQuery)
data_select <<- fetch(queryData, n=-1)
ggplot(data_select,aes(x=date_trunc,y=avg,group=report_id,colour=report_id))+geom_line()
require(plyr)
sum<-ddply(data_select,'date_trunc',summarize,tot=sum(avg))
ggplot(sum,aes(x=date_trunc,y=tot))+geom_line()+geom_smooth()

ggplot(data_select,aes(x=vehicle_count))+geom_histogram()

ggplot(data_select,aes(x=vehicle_count))+ stat_ecdf(geom = "step")
