
library(plyr)
library(ggplot2)
library(data.table)
library(parallel)
library(RPostgreSQL)
library(rgeos)
library(reshape)
devtools::install_github("ricardo-bion/ggradar",  dependencies=TRUE)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)
#setwd("~/svn/CorrelationMonitoring/CorrelationSystem")
setwd("~/svn/CityPulseDevelopment/trunk/A4.3/CorrelationMonitoring/CorrelationSystem")

source("util.R")
source("composite_m_lib.R")
all_p <- readRDS(file=paste0('./results/','results_flow_try_all_more5_2016-06-15.RDS'))
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

g_min5 <- ggplot(data=result.df) + geom_point(aes(x=speed_delta, y=speed_min5,color=confidence))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min5
#ggsave(g_min5, filename='./results/plots/flow_speed_impact_on_events_min_5.pdf', units="cm", width=30,height=19)
g_min1 <- ggplot(data=result.df) + geom_point(aes(x=speed_delta, y=speed_min1,color=confidence))#+scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn"))
g_min1

g_min1_heat <- ggplot(data=result.df,aes(x=speed_delta, y=speed_min1)) +geom_bin2d()+theme_bw()+#  stat_binhex()+#+ geom_bin2d()
  #  scale_fill_gradient(name = "Occurence", trans = "log", breaks = c(10,100,1000,10000), labels = c(10,100,1000,10000))+
  scale_fill_gradientn(name = "Occurence(Count)", trans = "log", breaks = c(10,100,1000,10000), labels = c(10,100,1000,10000),colours=c("yellow","blue","black"))+
  labs(x = "Difference (CurrentSpeed-FreeFlowSpeed, TomTom)")+
  labs(y = "Max. Speed Change (Irregular_DCT Component, ODAA)")+
  theme(legend.justification=c(0,1), legend.position=c(0,1),axis.text.x = element_text(angle = 90, hjust = 1),legend.background = element_rect(colour = "black"))
g_min1_heat
#ggsave(g_min1_heat, filename='./results/plots/Paper_flow_speed_impact_on_events_min_1_heatmap.pdf', units="cm", width=15,height=15)


100/count(result.df)*count(result.df[result.df$speed_min1<=0,])
100/count(result.df)*count(result.df[result.df$speed_min1<(-15),])
100/count(result.df)*count(result.df[result.df$speed_min1>=0,])

g_min1_heat_hex <- ggplot(data=result.df,aes(x=speed_delta, y=speed_min1)) + stat_binhex()+#+ geom_bin2d()
  theme_bw()+
  scale_fill_gradientn(name = "Occurence(Count)", trans = "log", breaks = c(10,100,1000,10000), labels = c(10,100,1000,10000),colours=c("yellow","blue","black"))+
  labs(x = "Difference (CurrentSpeed-FreeFlowSpeed, TomTom)")+
  labs(y = "Max. Speed Change (Irregular_DCT Component, ODAA)")+
  theme(legend.justification=c(0,1), legend.position=c(0,1),axis.text.x = element_text(angle = 90, hjust = 1),legend.background = element_rect(colour = "black"))
g_min1_heat_hex
ggsave(g_min1_heat_hex, filename='./results/plots/Paper_flow_speed_impact_on_events_min_1_heatmap_hex.pdf', units="cm", width=16,height=16)

#marginal density of x - plot on top
plot_top <- ggplot(result.df, aes(speed_delta)) + theme_bw()+
  geom_histogram(alpha=1,colour = "darkgreen", fill = "white", binwidth = 1) + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none",axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())

plot_top
#marginal density of y - plot on the right
plot_right <- ggplot(result.df, aes(speed_min1)) + theme_bw()+
  geom_histogram(alpha=1,colour = "darkgreen", fill = "white", binwidth = 1) + 
  coord_flip() + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1),axis.text.y=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank()) 
plot_right
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
library(gridExtra)

gg_heat_full <-grid.arrange(plot_top, empty, g_min1_heat, plot_right, ncol=2, nrow=2, widths=c(6, 1), heights=c(1, 6))
gg_heat_full
ggsave(gg_heat_full, filename='./results/plots/Paper_flow_speed_impact_on_events_min_1_heatmap_full.pdf', units="cm", width=15,height=15)

gg_heat_full_hex <-grid.arrange(plot_top, empty, g_min1_heat_hex, plot_right, ncol=2, nrow=2, widths=c(6, 1), heights=c(1, 6))
gg_heat_full_hex
ggsave(gg_heat_full_hex, filename='./results/plots/Paper_flow_speed_impact_on_events_min_1_heatmap_full_hex.pdf', units="cm", width=15,height=15)



#smoothScatter(result.df[,c('speed_delta','speed_min1')])


g_min1_heat <- ggplot(data=result.df,aes(x=speed_delta, y=speed_min1)) +  stat_binhex(aes(fill=log(..count..)))#+ geom_bin2d()
g_min1_heat


ggplot(result.df, aes(x=speed_delta, y=speed_min1)) +
  geom_point(shape=1, aes(color = factor(cond))) +
  geom_subplot2d(aes(xvar, yvar,
                     subplot = geom_bar(aes(cond, ..count.., fill = cond))),
                 bins = c(15,15), ref = NULL, width = rel(0.8), ply.aes = FALSE)  
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
with(result.df, {
  scatterplot3d(speed_delta,   # x axis
                time_delta,     # y axis
                import_time_stamp,    # z axis
                main="3-D Scatterplot Example 1")
})
with(result.df, {
  scatterplot3d(import_time_stamp,    # z axis
                speed_delta,   # x axis
                time_delta,     # y axis
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
                       import_time_stamp,
                       y,      # x y and z axis
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

