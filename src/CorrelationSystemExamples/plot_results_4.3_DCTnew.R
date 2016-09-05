library(grid)
library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
library(rgeos)
setwd("~/svn/CityPulseDevelopment/trunk/A4.3/CorrelationMonitoring/CorrelationSystem")
source("util.R")
source("composite_m_lib.R")

theme_paper <- function() {
  theme_bw(base_size=14) %+replace%
    theme(
      legend.position="bottom",
      legend.title=element_blank(),
      legend.background=element_rect(colour="grey75", fill="white", size=0.2),
      legend.key=element_rect(colour="white"),
      panel.border=element_rect(fill=NA, colour="black"),
      panel.grid.major=element_line(colour="grey75", size=0.2),
      panel.grid.minor=element_line(colour="grey90", size=0.2),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y = element_text(size=24, angle = 90)
    )
}

no_cores <- detectCores() - 1
idColumn = "report_id"
timestampColumn = "time_stamp"
dataColumns = c("vehicle_count", "avg_speed")
identifier = c("vehicleCount", "averageSpeed")
INPUTTYPES = c("raw", "dct", "random", "random_dct")
rate = 5*60
dctFilter = 21
daytime=60*60*24
weektime=60*60*24*7
weeks=6
num_sensors <- 10 # to compare
p1 <- readRDS(file='./results/PaperDecomposition_56_1.RDS')
pts <- p1$timeseries
j=1
    pts1 <- pts[[j]]
    dfp <- data.frame(pts1$seasonal$averageSpeed)
    dd <- data.frame(pts1$data$time_stamp)
    dd[,'raw'] <- data.frame(pts1$data$averageSpeed_raw)
    dd[,'random'] <- data.frame(pts1$data$averageSpeed_random)
    #dd[,'random_dct'] <- data.frame(pts1$data$averageSpeed_random_dct)
    
    selected_rows <- nrow(dd)-nrow(dfp):nrow(dd)
    dd_week <- dd[(nrow(dd)-nrow(dfp)+1):(nrow(dd)),]
    dd_week[,'seasonal'] <- dfp[,1]
    
    colnames(dd_week) <- c('Date','Raw','Irregular','Seasonal')
    dd_week = dd_week[,c('Date','Raw','Seasonal','Irregular')]
    meltdf = melt(dd_week,id='Date')
    et=p1$flow_data$import_time_stamp
    event_time <- as.POSIXct(et)
    
    free_flow = p1$flow_data$free_flow_speed
    current_flow = p1$flow_data$current_speed
    delta_flow =current_flow-free_flow
    #decomp_plot <- ggplot(meltdf,aes(x=Date,y=value,colour=variable,group=variable)) + geom_line(size=1.1) +
    decomp_plot <- ggplot(meltdf,aes(x=Date,y=value,group=variable)) + geom_line() +
      theme_bw() +
      ylab("Average Vehicle Speed (km/h)") + xlab("Date") + 
      ggtitle(paste("ODAA TS Decomposition -> TomTom FreeFlow(km/h):",free_flow,"CurrentFlow(km/h)",current_flow))+
      facet_grid(variable ~ . , scales = "free") +
      theme(legend.position = "none")+
      geom_vline(aes(xintercept=as.numeric(event_time)), linetype=2, colour="black")
    #geom_text(aes(x=event_time, label=paste('Flow:',delta_flow,"(km/h)"), y=0), colour="blue", angle=90, text=element_text(size=8)) 
    #geom_text(aes(x=event_time, label='\nFlow', y=0), colour="blue", angle=90, text=element_text(size=8)) 
    decomp_plot
    
    ggsave(decomp_plot, file=paste0('results/plots/Paper_V5_Decompose.pdf'),width = 8,height = 6)
    
    #ggsave(decomp_plot, file=paste0('results/Flow_AverageSpeedDecomposition_',i,'_',j,'.pdf'),width = 12,height = 12)












type <- 3
factor <- 9

size= length(dd_week[,'Irregular'])
df <- data.frame(1:size)
df[,'b'] <- data.frame(dd_week[,'Irregular'])

  
colnames (df) <- c('a','b')
#df <- data.frame(a=sample(size), b=1:size)
#df <- df[order(df[,1]), ]

# df[,2] <-df[,2]/size*4 + cos((1:size)/size*4*pi) + sin((1:size)/size*8*pi)
# df[,2] <- df[,2] - min(df[,2])
# df[,2] <- df[,2] / max(df[,2])
#plot(df[,2], type='l')
plot(df[,2])

df[,3] <- DCT(df[,2], type=type)
#plot(df[,3])


df[,4] <- rep(1, nrow(df))
s <- floor(nrow(df)/factor)
df[(s+1):(2*s), 4] <- (cos((1:s)/s * pi) + 1)/2
df[(2*s):nrow(df), 4] <- 0
#plot(df[,4], type='l')


df[,5] <- df[,3] * df[,4]
#plot(df[,5])

df[,6] <- IDCT(df[,5], type=type)
#plot(df[,6], type='l')
colnames(df)=c('x','Irregular','DCT-A','DCT-B','DCT-C','Irregular-DCT')
melt.df = melt(df, id='x')

gg_dct <- ggplot(melt.df,aes(x=x,y=value))+ 
  geom_line(data = subset(melt.df, (variable=='Irregular'|variable=='DCT-B'|variable=='Irregular-DCT'))) +
  geom_point(data = subset(melt.df, (variable=='DCT-A'|variable=='DCT-C')),size=0.5)+
  theme_bw() +
  ylab("km/h                  Frequency                  Lowpass                  Frequency                  km/h") + xlab("Time Series Data Point") + 
  #ggtitle(paste("ODAA TS Decomposition -> TomTom FreeFlow(km/h):",free_flow,"CurrentFlow(km/h)",current_flow))+
  facet_grid(variable ~ ., scales = "free") +
  #facet_wrap(~variable ,nrow=5, scales = "free") +
  theme(legend.position = "none")
  #geom_vline(aes(xintercept=as.numeric(event_time)), linetype=2, colour="black")
gg_dct
ggsave(gg_dct, filename='./results/plots/Paper_V5_Decompose_DCT.pdf', width=8,height=8)



