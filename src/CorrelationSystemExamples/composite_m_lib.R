suppressMessages(library(data.table))
suppressMessages(library(parallel))
suppressMessages(library(plyr))

parallel_prepare <- function(no_cores){
  cl <- makeCluster(no_cores)
  clusterExport(cl, "queryNewData")
  clusterExport(cl, "calculateReport")
  clusterExport(cl, "getDataPeriod.complete")
  clusterExport(cl, "na.fill")
  clusterExport(cl, "dbConnect")
  clusterExport(cl, "reports")
  clusterExport(cl, "idColumn")
  clusterExport(cl, "timestampColumn")
  clusterExport(cl, "dataColumns")
  clusterExport(cl, "getDataPeriod")
  clusterExport(cl, "correctRate")
  clusterExport(cl, "rbindlist")
  clusterExport(cl, "fillGaps.complete")
  clusterExport(cl, "na.locf")
  clusterExport(cl, "fillGaps")
  clusterExport(cl, "join")
  clusterExport(cl, "rbind.fill")
  clusterExport(cl, "identifier")
  clusterExport(cl, "seasonalNonSeries")
  clusterExport(cl, "timestampToSeasonalOffset")
  clusterExport(cl, "filterHighFreq")
  clusterExport(cl, "DCT")
  clusterExport(cl, "dctFilter")
  clusterExport(cl, "IDCT")
  return(cl)
}

queryReports_par <- function (cl, reports, fromTimestamp=NULL, toTimestamp=NULL){
  p= parLapply(cl, reports, calculateReport, fromTimestamp=fromTimestamp, toTimestamp=toTimestamp)
  return(p)
}

queryReports_seq <- function (reports, fromTimestamp=NULL,toTimestamp=NULL){
  print("seq")
  p= lapply(reports, calculateReport, fromTimestamp=fromTimestamp,toTimestamp=toTimestamp)
  return(p)
}
parallel_stop <- function(cl){
  stopCluster(cl)
}

queryNewData <- function(dbname, idColumn, timestampColumn, dataColumns, reportId, fromTimestamp=NULL, toTimestamp=NULL) {
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(PostgreSQL(), user= "citypulse", password="xxxx", dbname=dbname, port=5432, host="localhost")
  
  whereClause <- paste0(" WHERE report_id=", reportId)
  
  if (! is.null(fromTimestamp)) {
    whereClause = paste0(whereClause, " AND ", timestampColumn, " >= ", format(fromTimestamp, "'%Y-%m-%d 00:00:00'"))
  }
  if (! is.null(toTimestamp)) {
    whereClause = paste0(whereClause, " AND ", timestampColumn, " <= ", format(toTimestamp, "'%Y-%m-%d 00:00:00'"))
  }
  
  sqlQuery <- paste0("SELECT ", paste0(c(idColumn, timestampColumn, dataColumns), collapse=','), " FROM aarhus_data", whereClause, " ORDER BY ", timestampColumn);
  print(sqlQuery)
  queryData <- dbSendQuery(con, sqlQuery)
  data_select <<- fetch(queryData, n=-1)
  
  print(paste0("Got ", nrow(data_select), " datasets"))
  
  dbDisconnect(con)
  
  return (data_select)
}

calculateReport <- function(reportId, fromTimestamp=NULL, toTimestamp=NULL){
  data <- queryNewData('cp_traffic', idColumn, timestampColumn, dataColumns, reportId, fromTimestamp, toTimestamp)
  print(paste("1"))
  # Convert columns
  data <- transform(data, vehicleCount = as.integer(vehicle_count))
  data <- transform(data, averageSpeed = as.integer(avg_speed))
  
  # Round to full minutes
  data[, timestampColumn] <- as.POSIXct(round(data[, timestampColumn] - (as.integer(data[, timestampColumn]) %% 60), "mins"))
  data <- data[! duplicated(data[, timestampColumn]), ]
  
  data[, "day"] <- factor(format(data[, timestampColumn], "%Y %m %d"))
  data[, "period"] <- getDataPeriod.complete(data, timestampColumn=timestampColumn, splitColumn="day")
  minRate <- min(data[, "period"])
  
  data_ts <- split(data, data[, "period"])
  data_ts <- lapply(names(data_ts), function (period) {
    data <- data_ts[[period]]
    period <- as.integer(period)
    
    data[, timestampColumn] <- data[, timestampColumn] - (as.integer(data[, timestampColumn]) %% period)
    data <- data[! duplicated(data[, timestampColumn]), ]
    
    return (data)
  })
  data <- data.frame(rbindlist(data_ts), stringsAsFactors = FALSE)
  data <- data[order(data[, timestampColumn]), ]
  
  # Fill missing data by gaps up to 3*period
  data <- fillGaps.complete(data, periodColumn="period", timestampColumn=timestampColumn)
  rownames(data) <- NULL
  
  # Order and remove uneeded columns
  result <- data[order(data[, timestampColumn]), c(idColumn, timestampColumn, identifier)]
  
  # Save raw data
  colnames(result)[2:ncol(result)] <- c("time_stamp", paste0(identifier, "_raw"))
  analysis <- result
  
  # Build and save random data
  colnames(result)[3:ncol(result)] <- identifier
  data.seasonal <- seasonalNonSeries(result, identifier, timestampColumn, minRate)
  data.seasonal[, identifier] <- apply(data.seasonal[, identifier], 2, filterHighFreq, c=dctFilter)
  
  weekFactor = factor(format(result[, timestampColumn], "%Y %W"))
  result_random <- as.data.frame(rbindlist(lapply(split(result, weekFactor), function(r) {
    data.seasonal <- data.seasonal[timestampToSeasonalOffset(r[, timestampColumn], minRate), ]
    r[,identifier] <- r[,identifier] - t(t(data.seasonal[,identifier]) * (1 + apply(r[,identifier] - data.seasonal[,identifier], 2, mean)/apply(data.seasonal[,identifier], 2, mean)))
    
    return (r)
  })))
  
  colnames(result_random)[3:ncol(result_random)] <- paste0(identifier, "_random")
  analysis[,5:6] <- result_random[,3:4] #saveRDS(result_random, tempFilenames[INPUTTYPES == "random"])
  
  
  # Build and save random_dct data
  colnames(result_random)[3:ncol(result_random)] <- identifier
  result_random[, identifier] <- apply(result_random[, identifier], 2, filterHighFreq, c=dctFilter)
  colnames(result_random)[3:ncol(result_random)] <- paste0(identifier, "_random_dct")
  #analysis_random_dct <- result_random  #saveRDS(result_random, tempFilenames[INPUTTYPES == "random_dct"])
  analysis[,7:8] <- result_random[,3:4]
  
  # Built and save dct data
  result[, identifier] <- apply(result[, identifier], 2, filterHighFreq, c=dctFilter)
  colnames(result)[3:ncol(result)] <- paste0(identifier, "_dct")
  #analysis_dct <- result#  saveRDS(result, tempFilenames[INPUTTYPES == "dct"])
  analysis[,9:10] <- result[,3:4]
  
  
  # Built and save dct_random data
  colnames(result)[3:ncol(result)] <- identifier
  data.seasonal <- seasonalNonSeries(result, identifier, timestampColumn, minRate)
  data.seasonal[, identifier] <- apply(data.seasonal[, identifier], 2, filterHighFreq, c=dctFilter)
  
  weekFactor = factor(format(result[, timestampColumn], "%Y %W"))
  result_random <- as.data.frame(rbindlist(lapply(split(result, weekFactor), function(r) {
    data.seasonal <- data.seasonal[timestampToSeasonalOffset(r[, timestampColumn], minRate), ]
    r[,identifier] <- r[,identifier] - t(t(data.seasonal[,identifier]) * (1 + apply(r[,identifier] - data.seasonal[,identifier], 2, mean)/apply(data.seasonal[,identifier], 2, mean)))
    
    return (r)
  })))
  
  colnames(result_random)[3:ncol(result_random)] <- paste0(identifier, "_dct_random")
  #analyis_dct_random <- result_random #saveRDS(result_random, tempFilenames[INPUTTYPES == "dct_random"])
  analysis[,11:12] <- result_random[,3:4]
  
  
  # Build and save dct_random_dct data
  colnames(result_random)[3:ncol(result_random)] <- identifier
  result_random[, identifier] <- apply(result_random[, identifier], 2, filterHighFreq, c=dctFilter)
  colnames(result_random)[3:ncol(result_random)] <- paste0(identifier, "_dct_random_dct")
  #analysis_dct_random_dct <- result_random #saveRDS(result_random, tempFilenames[INPUTTYPES == "dct_random_dct"])
  analysis[,13:14] <- result_random[,3:4]
  
  report_info <- list('seasonal'=data.seasonal, 'data'=analysis)
  return(report_info)
}