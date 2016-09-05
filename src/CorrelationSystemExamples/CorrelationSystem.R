library(plyr)
library(ggplot2)
library(data.table)
library(parallel)

library(RPostgreSQL)

setwd("~/svn/CorrelationMonitoring/CorrelationSystem")
source("util.R")
source("composite_m_lib.R")

weektime=60*60*7

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


cl <- parallel_prepare(no_cores)
startTime = Sys.time()-(12*weektime)
p <- queryReports_par(cl,reports[c(4,8,10,22,400)],startTime)
parallel_stop(cl)

p


# can be plotted with plot_results_1.R

#test
reports[3]
startTime = Sys.time()-(12*weektime)
startTime

queryReports_seq(cl,reports[1:12],startTime)




gp <- ggplot(data=analysis) + geom_line( aes(x=time_stamp, y=averageSpeed_raw)) +
  geom_line(aes(x=time_stamp, y=averageSpeed_random_dct), color="red", alpha=0.5) 
#geom_line(aes(x=r[, timestampColumn], y=(r[, "averageSpeed"]-week.seasonal[, "averageSpeed"])), color="green", alpha=0.5)
gp

  
  # Test plots
  if (TRUE) {
    weekNum <- 60
    idx = as.integer(weekFactor) == weekNum
    
    tsOffset = timestampToSeasonalOffset(result[, timestampColumn], minRate)
    week.seasonal <- data.frame("vehicleCount" = as.numeric(data.seasonal[,"vehicleCount"])[tsOffset],
                                "averageSpeed" = as.numeric(data.seasonal[,"averageSpeed"])[tsOffset])
    
    week.seasonal[, timestampColumn] <- result[, timestampColumn]
    
    data.split <- matrix(sort(c(1, which(diff(result[, "time_stamp"]) > 2*60*60), which(diff(result[, "time_stamp"]) > 2*60*60)+1, nrow(result))), nrow=2)
    #data.split <- lapply(1:ncol(data.split), function(c) return (data.split[1,c]:data.split[2,c]))
    data.split <- unlist(lapply(1:ncol(data.split), function(c) return (rep(c, data.split[2,c]-data.split[1,c]+1))))
    
    lapply(split(result, data.split), function(r) {
      #r[, identifier] <- apply(r[, identifier], 2, filterHighFreq, c=dctFilter)
      
      #data.seasonal <- seasonalNonSeries(r, identifier, timestampColumn, minRate)
      #data.seasonal[, identifier] <- apply(data.seasonal[, identifier], 2, filterHighFreq, c=dctFilter)
      #tsOffset = timestampToSeasonalOffset(r[, timestampColumn], rate)
      
      #week.seasonal <- data.frame("vehicleCount" = as.numeric(data.seasonal[,"vehicleCount"])[tsOffset],
      #                            "averageSpeed" = as.numeric(data.seasonal[,"averageSpeed"])[tsOffset])
      #week.seasonal[, timestampColumn] <- r[, timestampColumn]
      week.seasonal <- join(r[, 1:2], week.seasonal)
      
      dTS <- decompose(ts(r[, "averageSpeed"], frequency = minRate))
      
      gp <- ggplot(data=analysis) + geom_line(aes(x=time_stamp, y=averageSpeed)) #+
        geom_line(aes(x=time_stamp, y=averageSpeed), color="red", alpha=0.5) +
        geom_line(aes(x=r[, timestampColumn], y=(r[, "averageSpeed"]-week.seasonal[, "averageSpeed"])), color="green", alpha=0.5)
      #geom_line(aes(x=r[, timestampColumn], y=dTS$random), color="pink", alpha=0.5)
      gp
      #ggsave(filename=paste0("/run/shm/averageSpeed_", r[1, timestampColumn], ".png"), plot=gp, width=35, height=9, units = "in", dpi=100)
    })
    
    
    data.seasonal <- seasonalNonSeries(result, identifier, timestampColumn, minRate)
    data.seasonal[, identifier] <- apply(data.seasonal[, identifier], 2, filterHighFreq, c=dctFilter)
    #seasonal.mean <- apply(data.seasonal[, identifier], 2, mean)
    #seasonal.sd <- apply(data.seasonal[, identifier], 2, sd)
    #seasonal.mean <- apply(sapply(split(data.seasonal[identifier], as.integer((0:(nrow(data.seasonal -1)-1)/(nrow(data.seasonal)/7)))), function(d) { apply(d, 2, mean) }), 1, mean)
    
    tsOffset = timestampToSeasonalOffset(result[, timestampColumn], minRate)
    data.seasonal <- data.seasonal[tsOffset, ]
    
    for (i in identifier) {
      data.seasonal[,i] <- data.seasonal[,i] * (1 + mean(result[,i] - data.seasonal[,i])/mean(data.seasonal[,i]))
    }
    
    dayFactor = factor(format(result[, timestampColumn], "%Y %m %d"))
    weekFactor = factor(format(result[, timestampColumn], "%Y %W"))
    day.mean <- as.data.frame(t(sapply(split(result[, identifier], dayFactor), function(d) { apply(d, 2, mean) })))
    week.mean <- data.frame(t(sapply(split(day.mean, sapply(split(as.integer(weekFactor), dayFactor), function(d) { d[[1]] })), function(d) { apply(d, 2, mean) })))
    day.mean <- day.mean[dayFactor, ]
    week.mean <- week.mean[weekFactor, ]
    
    #week.mean <- as.data.frame(t(sapply(split(result[, identifier], weekFactor), function(d) { apply(d, 2, mean) })))
    #week.mean <- week.mean[weekFactor, ]
    
    for (i in identifier) {
      gp <- ggplot() + xlab("Timestamp") + ylab("Value") +
        geom_line(aes(x=result[idx, timestampColumn], y=result[idx, i], color="Input data")) +
        geom_line(aes(x=result[idx, timestampColumn], y=week.seasonal[idx, i], color="Seasonal data")) +
        geom_line(aes(x=result[idx, timestampColumn], y=rep(mean(data.seasonal[, i]), sum(idx)), color="Seasonal data mean"), size=1.5) +
        geom_line(aes(x=result[idx, timestampColumn], y=rep(temp[i], sum(idx))), color="pink", size=1.5) +
        geom_line(aes(x=result[idx, timestampColumn], y=week.mean[idx, i], color="Input data weekly mean"), size=1.5) +
        geom_line(aes(x=result[idx, timestampColumn], y=day.mean[idx, i], color="Input data daily mean"), size=1.5) +
        scale_color_manual(values = c("Input data"="black",
                                      "Seasonal data"="red",
                                      "Seasonal data mean"="green",
                                      "Input data weekly mean"="blue",
                                      "Input data daily mean"="yellow"))
      print(gp)
    }
    
    # Normalize seasonal data
    #for (i in identifier) data.seasonal[, i] <- (data.seasonal[, i] - mean(data.seasonal[, i])) / sd(data.seasonal[, i])
    
    dayFactor = factor(format(result[, timestampColumn], "%Y %m %d"))
    day.mean <- as.data.frame(t(sapply(split(result[, identifier], dayFactor), function(d) { apply(d, 2, mean) })))
    #day.mean <- as.data.frame(t(sapply(split(result[, identifier], dayFactor), function(d) { c(apply(d, 2, mean), apply(d, 2, sd)) })))
    #colnames(day.mean)[1:2] <- paste0(colnames(day.mean)[1:2], "_mean")
    #colnames(day.mean)[3:4] <- paste0(colnames(day.mean)[3:4], "_sd")
    
    weekFactor = factor(format(result[, timestampColumn], "%Y %W"))
    week.mean <- data.frame(t(sapply(split(day.mean, sapply(split(as.integer(weekFactor), dayFactor), function(d) { d[[1]] })), function(d) { apply(d, 2, median) })))
    week.mean <- week.mean[weekFactor, ]
    
    tsOffset = timestampToSeasonalOffset(result[, timestampColumn], rate)
    week.seasonal <- data.frame("vehicleCount" = as.numeric(data.seasonal[,"vehicleCount"])[tsOffset]*week.mean[, "vehicleCount_sd"] + week.mean[, "vehicleCount_mean"],
                                "averageSpeed" = as.numeric(data.seasonal[,"averageSpeed"])[tsOffset]*week.mean[, "averageSpeed_sd"] + week.mean[, "averageSpeed_mean"])
  }
  
  # Test plots
  if (TRUE) {
    weekNum <- 50
    idx = as.integer(weekFactor) == weekNum
    
    i = identifier[1]
    for (i in identifier) {
      gp <- ggplot() + xlab("Timestamp") + ylab("Value") +
        geom_line(aes(x=result[idx, timestampColumn], y=result[idx, i], color="Input data")) +
        geom_line(aes(x=result[idx, timestampColumn], y=week.seasonal[idx, i], color="Seasonal data")) +
        scale_color_manual(values = c("Input data"="black",
                                      "Seasonal data"="red"))
      print(gp)
      
      plot(1:sum(idx), result[idx, i] - week.seasonal[idx, i], type='l')
      
      gp <-ggplot() + geom_point(aes(x = result[, timestampColumn], y = week.mean[,paste0(i, "_mean")])) +
        geom_line(aes(x = result[c(1, nrow(result)), timestampColumn], y = rep(seasonal.mean[i], 2)), color="red")
      print(gp)
    }
  }
  
  #saveRDS(result, tempFilename)
  
  return (result[, "time_stamp"])
  #return (result)
