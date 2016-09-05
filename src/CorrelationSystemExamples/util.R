# Util functions
suppressMessages(library(zoo))
suppressMessages(library(fftw))


useTimezone = "CET"


# Converts a string date to a POSIXct date
convertTimestamp <- function(strTimestamp) {
  if (class(strTimestamp)[1] == "POSIXct") {
    return (strTimestamp)
  }

  if (length(grep("T", strTimestamp)) > 0) {
    mask = "%Y-%m-%dT%H:%M:%S"
  } else if (length(grep(" ", strTimestamp)) > 0) {
    mask = "%Y-%m-%d %H:%M:%S"
  } else {
    mask = "%Y-%m-%d"
  }

  return (as.POSIXct(strptime(strTimestamp, mask)))
}


# Determines the next monday 00:00:00 for a given date
nextMonday <- function(ts, i=1, f=period) {
  d = 24*60*60 / f
  t <- as.integer(unlist(strsplit(strftime(ts[i], '%H,%M,%u'), ',', fixed=TRUE)))
  offset = 8*d - (t[3]*d + (t[1]*60+t[2])*60 / f)

  return (i+(offset %% (7*d)))
}


# Find folders for given timespan
findFolders <- function(startTS, endTS) {
  start <-  convertTimestamp(c("2014-02-13T11:30:00", "2014-06-09T05:50:00", "2014-08-01T07:50:00", "2014-10-01T01:40:00"))
  end <-    convertTimestamp(c("2014-06-09T05:35:00", "2014-09-17T02:45:00", "2014-09-30T23:55:00", "2014-11-13T10:45:00"))
  folder <- c("DataFebruaryJune2014", "DataJuneSeptember2014", "DataAugusteSeptember2014_Complete", "DataOctoberToMidNovember2014")
  s <- data.frame(start, end, folder)

  folders <- vector()

  if (startTS >= s$start[1] && endTS > startTS && endTS <= tail(s$end, 1)) {
    for (i in 1:length(s$folder)) {
      if (startTS >= s$start[i]) {
        folders[1] <- as.character(s$folder[i])
      } else if (startTS < s$start[i]) {
        if (endTS < s$start[i]) {
          break
        } else {
          endFolder <- as.character(s$folder[i])
          folders[length(folders)+1] <- endFolder

          if (endTS <= s$end[i]) {
            break
          }
        }
      }
    }
  }

  return (folders)
}


# Reads data from a given csv file
readTrafficFile <- function(datafile) {
  header=c("status","avgMeasuredTime","avgSpeed","extID","medianMeasuredTime","TIMESTAMP","vehicleCount","_id","REPORT_ID")
  con <- file(datafile)
  firstEntry <- strsplit(readLines(con, n=1), ',', fixed=TRUE)[[1]][1]
  close(con)

  if (firstEntry == header[1]) {
    data <- read.csv(datafile, sep=",", header=TRUE)
  } else {
    data <- read.csv(datafile, sep=",", header=FALSE, col.names=header)
  }

  # Convert timestamp
  data <- transform(data, TIMESTAMP=convertTimestamp(TIMESTAMP))

  return (data)
}


# Fill gaps in timeseries
fillGaps <- function(data, timestampCol, n) {
  if (! is.data.frame(data)) return (NA)
  if (nrow(data) < 2) return (data)

  if (TRUE) {
    dates <- data.frame(seq.POSIXt(head(data[, timestampCol],1), tail(data[, timestampCol],1), by=paste0(n, " secs")))
    names(dates) <- timestampCol
    data <- join(dates, data, by=timestampCol)
    for (i in 1:ncol(data)) {
      if (class(data[,i])[1] == "character" || class(data[,i])[1] == "factor") {
        data[,i] <- na.locf(data[,i])
      } else {
        data[,i] <- na.fill(data[,i], fill="extend")
      }
    }

  } else {
    temp <- zoo(1:length(data[, timestampCol]), data[, timestampCol])
    dates <-seq.POSIXt(head(index(temp),1), tail(index(temp),1), by=paste0(n, " secs"))
    temp <- na.locf(temp, xout=dates, rule=1)
  
    # Apply filled data and correct timestamps
    data <- data[temp,]
    data[, timestampCol] <- dates
  }

  return (data)
}


fillGaps.complete <- function(data, periodColumn, timestampColumn) {
  data_ts <- split(data, data[, periodColumn])

  data_ts <- lapply(names(data_ts), function (period) {
    data <- data_ts[[period]]
    period <- as.integer(period)

    data.split <- c(TRUE, diff(as.integer(data[, timestampColumn])) > (4*period))
    data.split[data.split] <- 1:sum(data.split)
    data.split[! data.split] <- NA
    data.split <- na.locf(data.split)

    data_s <- split(data, data.split)

    # Remove data sets with to few data
    data_s[sapply(data_s, function(d) nrow(d) <= 1)] <- NULL
    data_s <- lapply(data_s, fillGaps, timestampCol=timestampColumn, n=period)

    data <- rbind.fill(data_s)

    return (data)
  })

  data <- rbind.fill(data_ts)
  return (data)
}


# Calculates the mean of values between the bottom and top quartil of an vector x
quartilMean <- function(x) {
  n <- (length(x)+1)/4
  mean(sort(x)[ceiling(n):floor(3*n)])
}


# Calculate mean values over the next and previous o elements in x
meanOverN <- function(x, o=3) {
  c = 2*o+1
  l = length(x)

  t <- x[1:(l-(c-1))]
  for (i in 2:c) {
    t <- t + x[i:(l-(c-i))]
  }

  offset = array(NA, o)
  return (c(offset, (t / c), offset))
}


# Calculates the mean values over the previous c elements in x
meanOverPrevN <- function(x, c=3) {
  l = length(x)

  t <- x[1:(l-c)]
  for (i in 2:(c+1)) {
    t <- t + x[i:(l-(c-i+1))]
  }

  return (c(array(NA, c), (t / (c+1))))
}


# Filters high frequencies in x with dct
filterHighFreq <- function(x, c) {
  tx = DCT(as.double(x))
  s <- floor(length(tx)/c)

  #tx[(s+1):(2*s)] <- tx[(s+1):(2*s)] * c((s-1):0)/s
  tx[(s+1):(2*s)] <- tx[(s+1):(2*s)] * (cos((1:s)/s * pi) + 1)/2
  tx[(2*s):length(tx)] <- array(0, dim=(length(tx)-2*s+1))

  return (IDCT(tx))
}


# Creates a time series
createTimeseries <- function(x, t, f) {
  myts <- ts(x, start=c(nextMonday(t, 1)), frequency=f)
  mytsdc <- decompose(myts)

  #  s <- nextMonday(t, f)
  #  plot(type="l", x=t[s:(s+f)], xlab="Timestamp",
  #       y=mytsdc$seasonal[s:(s+f)], ylab="Seasonal")

  #  plot(type="l", x=t[s:(s+f)], xlab="Timestamp",
  #       y=mytsdc$random[s:(s+f)], ylab="Random")

  #  plot(type="l", x=t[s:(s+f)], xlab="Timestamp",
  #       y=mytsdc$trend[s:(s+f)], ylab="Trend")

  #  plot(type="l", x=t, xlab="Timestamp", y=mytsdc$seasonal, ylab="Seasonal")
  #  plot(type="l", x=t, xlab="Timestamp", y=mytsdc$trend, ylab="Trend")
  #  plot(type="l", x=t, xlab="Timestamp", y=mytsdc$random, ylab="Random")

  return (mytsdc)
}


# Calculatates a seasonal portion in x
seasonal <- function(x, ts) {
  s = 1 #nextMonday(ts)
  f = 7*24*60*60 / period
  e = s+f-1

  n = 0
  t <- array(0.0, f)
  while (e <= length(x)) {
    t <- t + x[s:e]

    s <- e+1
    e <- s+f-1
    n <- n+1
  }

  if (s <= length(x)) {
    r = x[s:length(x)]
    t[1:length(r)] <- (t[1:length(r)] + r) / (n+1)
  } else {
    r = vector()
  }

  if (n > 0) {
    if (length(r) > 0) {
      t[(length(r)+1):f] <- t[(length(r)+1):f] / n
    } else {
      t[1:f] <- t[1:f] / n
    }

    for (i in 1:(n-1)) {
      s = i*f+1
      e = s+f-1
      t[s:e] <- t[1:f]
    }

    if (length(r) > 0) {
      t[(e+1):length(x)] <- t[1:length(r)]
    }
  }

  return (t)
}


timestampToSeasonalOffset_old <- function(ts, rate) {
  r <- sapply(strsplit(format(ts, "%u %H %M", tz=useTimezone, usetz=FALSE), " "), function(i) {
    i = as.integer(i);
    return (((i[[1]]-1)*24*60*60 + i[[2]]*60*60 + i[[3]]*60) / rate + 1);
  })

  return (as.integer(r))
}

timestampToSeasonalOffset <- function(ts, rate) {
  ts <- as.POSIXlt(ts)
  r <- ((((ts[["wday"]]-1) %% 7)*24 + ts[["hour"]])*60 + ts[["min"]])*60 / rate +1

  return (as.integer(r))
}

# Calculatates a seasonal portion in data without serial timestamps
seasonalNonSeries <- function(data, identifierCols, timestampCol=NULL, rate=60) {
  weekRate = 7*24*60*60/rate

  if (is.null(timestampCol)) {
    timestampCol = "time_stamp"
  }

  if (class(data) != "data.frame" ||
      (! timestampCol %in% colnames(data)) ||
      sum(! identifierCols %in% colnames(data)) > 0) {
    return (NA)
  }

  data.offset <- timestampToSeasonalOffset(data[, timestampCol], rate)

  if (length(identifierCols) == 1) {
    t <- lapply(split(data[, identifierCols], factor(data.offset)), function(i) {
      i <- na.omit(i)
      c(sum(i), length(i))
      #c(median(i), sum(i), length(i))
    })

  } else {
    t <- lapply(split(data[, identifierCols], data.offset), function(i) {
      c(apply(i, 2, sum), nrow(i))
      #c(apply(i, 2, median), apply(i, 2, sum), nrow(i))
    })
  }

  # Fill missing values
  if (length(t) < weekRate) {
    idx = (1:weekRate)[-as.integer(names(t))]
    for (i in idx) {
      t[[as.character(i)]] <- rep(0, length(identifierCols)+1)
      #t[[as.character(i)]] <- rep(0, 2*length(identifierCols)+1)
    }
  }

  # Order and join data
  t <- t[order(as.integer(names(t)))]
  data.seasonal <- as.data.frame(matrix(unlist(t), nrow=weekRate, byrow=TRUE))
  colnames(data.seasonal) <- c(paste0(identifierCols, "_sum"), "count")
  for (s in identifierCols) {
    data.seasonal[, s] <- data.seasonal[, paste0(s, "_sum")] / data.seasonal[, "count"]
    data.seasonal[data.seasonal[, "count"] == 0, s] <- 0
  }

  #colnames(data.seasonal)[1:length(identifierCols)] <- paste0(identifierCols, "_mean")
  #colnames(data.seasonal)[(length(identifierCols)+1):(2*length(identifierCols))] <- paste0(identifierCols, "_sum")
  #colnames(data.seasonal)[2*length(identifierCols)+1] <- "count"

  #data.seasonal <- data.frame("count"=rep(-1, weekRate))
  #data.seasonal[, identifierCols] <- integer(weekRate)
  #data.seasonal <- data.seasonal[, c(2:ncol(data.seasonal), 1)]
  #for (i in 1:weekRate) {
  #  v = t[[as.character(i)]]
  #  if (! is.null(v)) data.seasonal[i,] <- v;
  #}

  return (data.seasonal)
}


# Converts n to i,j coordinates
NtoIJ <- function(n, len) {
  i = 1

  len = len-1
  while (n > len) {
    i = i+1
    n = n-len
    len = len-1
  }

  j = n+i
  return (c(i, j))
}


# Converts i,j coordinates to n
IJtoN <- function(i, j, len) {
  if (i == j) { return (0) }
  if (i < j) { return (IJtoN(j, i, len)) }

  i = i-j
  j = len-j+1
  n = (len*(len-1)/2) - (j*(j-1)/2) + i

  return (n)
}

# Transposes a data frame
transpose <- function(d) {
  if (length(d[,1]) == 0) {
    return (data.frame())
  }

  r <- data.frame('1'=vector(length=ncol(d)))[,-1]
#  for (name in rownames(d)) {
#    r[,name] <- d[name,]
#  }
  for (i in 1:length(d[,1])) {
    r[,i] <- d[i,]
  }
  
  colnames(r) <- c(1:length(r[1,]))
  return (r)
}

deg2rad <- function(a) {
  return (a * pi / 180);
}

rad2deg <- function(a) {
  return (a * 180 / pi);
}

absAngleDiff <- function(angle1, angle2) {
  abs(((angle1-angle2 + pi) %% (2*pi)) - pi)
}

# Calculates the euclidean distance between two geo points
euclideanDistance <- function(lat1, lng1, lat2, lng2) {
  r0 = 6371000;
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  c = deg2rad(lng2 - lng1);
  d = suppressWarnings(acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(c)))
  d[is.nan(d)] <- 0;

  return (d * r0);
}

amplify <- function(x, i=0, j=1) {
  if (i==0) {
    return (sin(x*pi - pi/2)/2 +0.5)
  } else {
    return ((i*x + j*(sin(x*pi - pi/2)/2 +0.5)) / (i+j))
  }
}

amplify.rev <- function(x, i=0, j=1) {
  if (i==0) {
    return (asin(x*2 - 1)/pi +0.5)
  } else {
    return ((i*x + j*(asin(x*2 - 1)/pi +0.5)) / (i+j))
  }
}



# Normalizes a vector to values between -1 and 1
normalize <- function(data) {
  data <- data - mean(data)
  return (data / sd(data) / 2)

  #return (data / max(mean(data[data >= 0]), -mean(data[data <= 0])) / 2)
  #return (data / max(abs(data)))
}


cIndex <- function(n) {
  #temp <- data.frame(a=unlist(lapply(1:n, function(i) rep(i,(n-1)))), b=unlist(lapply(1:n, function(i) (1:n)[-i])))
  unlist(lapply(1:n, function(i) c(rep(FALSE, i-1), rep(TRUE, n-i)) ))
}

# Sorts a given dataset by the first two rows containing
# the sensor ids
sortByIds <- function(data) {
  o <- order(data[,2])
  data <-data[o[order(data[o,1])], ]
  rownames(data) <- 1:nrow(data)
  
  return (data)
}


# Checks if the first two rows of two given datasets
# containing the sensor ids match
matchIds <- function(data1, data2) {
  return (nrow(data1) == nrow(data2) && sum(data1[,1:2] != data2[, 1:2]) == 0)
}


# Calculates the min for all element pairs in two vectors
minOverVector <- function(v1, v2) {
  apply(matrix(c(v1, v2), ncol=2, byrow=FALSE), 1, min)
}


# Splits a given vector or data.frame in n chunks
chunk <- function(data, n) {
  if (n == 1) return (data)
  if (n < 1) return (NA)

  if (is.data.frame(data) || is.matrix(data)) {
    return (split(data, floor((0:(nrow(data)-1))*n / nrow(data))))

  } else if (is.vector(data)) {
    return (split(data, floor((0:(length(data)-1))*n / length(data))))
  }

  return (NA)
}



correctRate <- (function() {
  possibleRates <- sort(c(1, 5, 30)*60)
  rates.mean <- (possibleRates[-1] + possibleRates[-length(possibleRates)])/2

  return (function(rate) {
    i <- 1
    for (r in rates.mean) {
      if (rate <= r) break
      i <- i+1
    }
    return (possibleRates[i])
  })
})()

getDataPeriod <- function(timestampData) {
  if (length(timestampData) < 2) return (NA)

  tsDiff <- diff(as.integer(timestampData))
  #rate = median(tsDiff)
  #rate <- round((tsDiff + 3*60) / (5*60))
  rate <- round(tsDiff / 60)*60

  #rate <- rate - (rate %% (5*60))
  #print(rate)
  #rate <- rate[rate %in% possibleRates]
  rate_count <- sapply(split(rate, rate), length)
  rate <- as.integer(names(which(rate_count >= length(rate)/8)[1]))

  return (correctRate(rate))
}

getDataPeriod.complete <- function(data, timestampColumn, splitColumn=NULL) {
  if (is.null(splitColumn)) {
    data.split <- factor(format(data[, timestampColumn], "%Y %m %d"))
  } else {
    data.split <- factor(data[, splitColumn])
  }

  data.period <- na.fill(unlist(lapply(split(data[, timestampColumn], data.split), getDataPeriod)), "extend")
  return (data.period[data.split])
}
