library(plyr)
library(ggplot2)
#saveRDS(times,file = paste0('./results/','scalability_test_2_',Sys.Date(),'.RDS'))
plot_data <- readRDS(file = paste0('./results/','scalability_test_2_2016-06-06.RDS'))
plot_data <- readRDS(file = paste0('./results/','scalability_test_3_2016-06-08.RDS'))
plot_data <- readRDS(file = paste0('./results/','scalability_test_5_2016-07-12.RDS'))
plot_data <- readRDS(file = paste0('./results/','scalability_test_6_2016-07-12.RDS'))



# plot_data[,'seconds']<-plot_data[,'seconds']*5
plot_data[,'weeks']<-as.factor(plot_data[,'weeks'])
colnames(plot_data)<-c('Sensors','Seconds','Weeks','run')
g_box <- ggplot(data=plot_data) + 
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position=c(0,1),legend.background = element_rect(colour = "black"))+
  xlim(c(0,50))+
  geom_boxplot(outlier.shape=16, outlier.size=0.7, notch=FALSE, 
               aes(x=Sensors,y=Seconds,group=interaction(Sensors,Weeks),color=Weeks)) #+scale_x_log10()+scale_y_log10()
g_box
#ggsave(g_box, filename='./results/plots/timeseries_processing_box_nolog.pdf', units="cm", width=30,height=19)
ggsave(g_box, filename='./results/plots/Paper_timeseries_processing_box_nolog_v2.pdf', units="cm", width=14,height=10)

g_box_log <- ggplot(data=plot_data) +  theme_bw()+
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, 
               aes(x=sensors,y=seconds,group=interaction(sensors,weeks),color=as.character(weeks))) +
                scale_x_log10()+scale_y_log10()
g_box_log
#ggsave(g_box_log, filename='./results/plots/timeseries_processing_box_log.pdf', units="cm", width=30,height=19)


plot_data_12=plot_data[plot_data$weeks==12,][c('sensors','seconds','run')]
g_12 <- ggplot(data=plot_data_12) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, 
               aes(x=sensors,y=seconds,group=sensors,color=sensors))
g_12
ggsave(g_12, filename='./results/plots/timeseries_processing_12weeks_nolog.pdf', units="cm", width=30,height=19)

g_12_log <- ggplot(data=plot_data_12) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, 
               aes(x=sensors,y=seconds,group=sensors,color=sensors))+
              scale_x_log10()+scale_y_log10()
g_12_log
ggsave(g_12_log, filename='./results/plots/timeseries_processing_12weeks_log.pdf', units="cm", width=30,height=19)


times_mean <- ddply(plot_data, .(weeks, sensors), summarize, mean = round(mean(seconds), 2))
colnames(times_mean) <- c('weeks', 'sensors', 'seconds_mean')
times_mean[,'weeks']<-as.character(times_mean[,'weeks'])
g_median <- ggplot(data=times_mean)+geom_line(aes(x=sensors,y=seconds_mean, group=weeks, color=weeks))
g_median
ggsave(g_median, filename='./results/plots/timeseries_processing_median_nolog.pdf', units="cm", width=30,height=19)

g_median_log <- ggplot(data=times_mean)+geom_line(aes(x=sensors,y=seconds_mean, group=weeks, color=weeks))+
                  scale_x_log10()+scale_y_log10()
ggsave(g_median_log, filename='./results/plots/timeseries_processing_median_log.pdf', units="cm", width=30,height=19)


