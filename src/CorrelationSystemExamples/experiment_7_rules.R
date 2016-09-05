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
p  <- readRDS("./results/results_try_all_with_all_data_2016-06-27.RDS") 

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
      min(x$distances$dist),
      x$ex_time_seconds
    )
  })
), nrow=length(p), byrow=T),stringsAsFactors=FALSE)
colnames(result.df) <- c('severity','type','duration','length','speed_min5','speed_min1','speed_mean','speed_ts','count_min5','count_min1','count_mean','count_ts','count_raw','min_dist', 'execution_time_s')

result.df[result.df$severity==0,'severity_text']<-'0 Unknown'
result.df[result.df$severity==1,'severity_text']<-'1 Slow Traffic'
result.df[result.df$severity==2,'severity_text']<-'2 Queuing Traffic'
result.df[result.df$severity==3,'severity_text']<-'3 Stationary Traffic'
result.df[result.df$severity==4,'severity_text']<-'4 Closed'
summary(result.df)
# IMPORTANTREDUCTION to suit euclidean pattern
result.df <- result.df[result.df$min_dist<50,]


result.rules <- result.df[,c('severity','type', 'speed_min1','count_min1','count_raw')]
#result.rules <- result.df[,c('severity','type', 'count_raw')]

result.rules[,'severity'] <- as.factor(result.rules[,'severity'])
result.rules[,'type'] <- as.factor(result.rules[,'type'])
result.rules['count_raw'] <- result.rules['count_raw']*10
result.rules<-result.rules[!is.infinite(result.rules[,'count_raw']),]

result.rules['count_raw'] <- discretize(result.rules[,'count_raw'],method="interval", categories = 5)
result.rules['speed_min1'] <- discretize(result.rules[,'speed_min1'],method="interval", categories = 5)
result.rules['count_min1'] <- discretize(result.rules[,'count_min1'],method="interval", categories = 5)


summary(result.rules)

library(arules)
rules <- apriori(result.rules)
inspect(rules)



# rules with rhs containing "Survived" only
rules <- apriori(result.rules,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("severity=1", "severity=2", "severity=3", "severity=4"),
                                   default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#install.packages('arulesViz')
library(arulesViz)
plot(rules)

plot(rules, method="graph", control=list(type="items"))

plot(rules, method="paracoord", control=list(reorder=TRUE))


