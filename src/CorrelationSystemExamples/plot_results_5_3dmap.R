library(scatterplot3d)
library(ggplot2)
library(sp)
library(rgeos)
setwd("~/svn/CityPulseDevelopment/trunk/A4.3/CorrelationMonitoring/CorrelationSystem")


if (exists('con')){
  dbDisconnect(con)
  #dbClearResult(dbListResults(con)[[1]])
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="qPoRD3Zk", dbname="cp_traffic", port=5435, host="localhost")

library(rgdal)
mydsn="PG:dbname='cp_traffic'  host=localhost user=citypulse password=qPoRD3Zk port=5435"
#ogrListLayers(mydsn)
#ogrInfo(mydsn, "ways")
ways = readOGR(dsn=mydsn,layer="ways", verbose = TRUE,stringsAsFactors=FALSE)
forti_ways = fortify(ways)
#ggplot(data=forti_ways, aes(long, lat, group=group)) + 
#  geom_polygon(colour='black',fill='white') +theme_bw()



p  <- readRDS("/Users/danielk/experiments/results_try_all_with_all_data_2016-06-27.RDS") 

interesting_speed=c(7645, 7588,3153,2045)
mintimegraph = 1464297501
saveprint=FALSE
max_dist <- 200
#for(i in 1:length(p)){
for(i in interesting_speed){
  print(paste(i,"/", length(p)))
  near_sensors <- which(p[[i]]$distances$dist<max_dist)
  distances = p[[i]]$distances$dist[near_sensors]
  ids = p[[i]]$distances$report_id[near_sensors]
  location_p1 <- p[[i]]$distances$p1[near_sensors]
  location_p2 <- p[[i]]$distances$p2[near_sensors]
  d_list <- p[[i]]$datalist[near_sensors] #multiple item select from list
  result.df <- data.frame(lat=numeric(),lon=numeric(),date=as.POSIXct(character()), val= numeric(),origin=character())
  if((length(d_list)>1) ) {
     for (j in 1:length(d_list)){
       if(nrow(d_list[[j]])>1){
          #report_df <- data.frame(d_list[[j]]$time_stamp,d_list[[j]]$vehicleCount_raw)
          report_df <- data.frame(d_list[[j]]$time_stamp,d_list[[j]]$averageSpeed_random_dct)
          colnames(report_df) <- c('date','val')
          report_df['origin'] <- paste0('ODAA(',ids[j],')')
          wkt = readWKT(location_p1[j])
          report_df['lon'] <- wkt$x
          report_df['lat'] <- wkt$y
          colnames(report_df)
          result.df <- rbind(result.df,report_df[c("lat","lon", "date",   "val"  ,  "origin"   )])
       }
    }
    if (max(result.df$val)<=(-15) & nrow(result.df)>0 & !is.na(max(result.df$val))){
      lat <- as.numeric(p[[i]]$icon_coord[2])
      lon <- as.numeric(p[[i]]$icon_coord[1])
      val <- p[[i]]$severity*(-1)
      date <- p[[i]]$icon_data[['import_time_stamp']]
      origin <- paste0('ODAA(',p[[i]]$icon_data[['poi_id']],')')
      event.df <- data.frame(lat,lon,date,val,origin)
      result.df <- rbind(result.df,event.df)
      result.df$pcolor <- "black"
      col_delta <- max(result.df$val)/5
      result.df$pcolor[result.df$val>= 0] <- "blue"
      result.df$pcolor[result.df$val>= 1*col_delta] <- "darkgreen"
      result.df$pcolor[result.df$val>= 2*col_delta] <- "green"
      result.df$pcolor[result.df$val>= 3*col_delta] <- "yellow"
      result.df$pcolor[result.df$val>= 4*col_delta] <- "orange"
      result.df$pcolor[result.df$val>= 5*col_delta] <- "red"
      
      min_lat <- min(result.df$lat)
      min_lon <- min(result.df$lon)
      max_lat <- max(result.df$lat)
      max_lon <- max(result.df$lon)
      delta_lat = (max_lat-min_lat)*0.05
      delta_lon = (max_lon-min_lon)*0.05
      fortiselect <- forti_ways[forti_ways$long > min_lon-delta_lon & forti_ways$long < max_lon+delta_lon & forti_ways$lat > min_lat-delta_lat & forti_ways$lat < max_lat+delta_lat,]
      #fortiselect <- forti_ways[forti_ways$long > min_lon & forti_ways$long < max_lon & forti_ways$lat > min_lat & forti_ways$lat < max_lat,]
      
      #Z-Min DATE
      fortiselect$date=min(result.df$date)
      fortiselect$group <- as.numeric(fortiselect$group)
      min_time=min(result.df$date)
      max_time=max(result.df$date)
      time_2 = min_time+((max_time-min_time)/3)
      time_3 = min_time+((max_time-min_time)/3*2)
      times = c(min_time,time_2,time_3,max_time)
      strftime(times, format="%H:%M")
      z_ticks = c(min(result.df$date),max(result.df$date))
      
      
      aspect_ratio = delta_lon/delta_lat
      
      if(saveprint){
        png(paste0('results/3d/Speed_3dt_',i,'(',length(d_list),').png'),width = 1200, height = 1200, units = "px",pointsize = 18)
      }
      s3d <- scatterplot3d(event.df$lat,
                           event.df$lon,
                           event.df$date,      # x y and z axis
                           color='white', pch=19,        # circle color indicates no. of cylinders
                           type="p", 
                           main="Traffic Time Flow",
                           xlab="Latitude",
                           ylab="Longitude",
                           zlab="Time",
                           angle=45,
                           scale.x = aspect_ratio,
                           #z.ticklabs = strftime(times, format="%H:%M"),
                           col.axis = grey(.7),col.grid = "white",
                           xlim = c(as.numeric(min(result.df$lat)),as.numeric(max(result.df$lat))),
                           ylim = c(as.numeric(min(result.df$lon)),as.numeric(max(result.df$lon))),
                           zlim =c(as.numeric(min(result.df$date)),as.numeric(max(result.df$date)))
      )
      
      groupids <- unique(fortiselect$group)
      for (g in groupids){
        forti_sub_select <- fortiselect[fortiselect$group==g,]
        s3d$points3d(forti_sub_select$lat,
                     forti_sub_select$long,
                     forti_sub_select$date,      # x y and z axis
                     col=grey(.4), type='l'
        )
      }
      
      s3d$points3d(result.df$lat,
                   result.df$lon,
                   result.df$date,      # x y and z axis
                   col=result.df$pcolor, pch=19,        # circle color indicates no. of cylinders
                   type="h"# ,lty.hplot=2    
      )
      s3d.coords <- s3d$xyz.convert(result.df$lat, result.df$lon, result.df$date)
      text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
           labels=as.character(round(result.df$val,2)),       # text to plot
           pos=c(4), cex=.5)                  # shrink text 50% and place to right of points)
      # add the legend
      #legend("topleft", inset=.05,      # location and inset
      #       bty="n", cex=.5,              # suppress legend box, shrink text 50%
      #       title="Avg. Speed Change",
      #       c("<10","<= -10", "<= -20", "<= -30", "<= -40", "<= -50"), fill=c("blue","darkgreen","green","yellow","orange","red"))
      
      #s3d$plane3d(c(as.numeric(event.df$date-(15*60)),event.df$lon,event.df$lat),col='red', lty='dotted')
      #s3d$plane3d(c(as.numeric(event.df$date-(15*60)),event.df$lon,event.df$lat),col='red', lty='dotted')
      #s3d$plane3d(c(as.numeric(event.df$date),event.df$lon,event.df$lat),col='red', lty='dotted')
      s3d$plane3d(c(as.numeric(event.df$date),0,0),col='red', lty='dotted')
      s3d$points3d(event.df$lat,
                   event.df$lon,
                   event.df$date,      # x y and z axis
                   col='black', pch=1)
     
      if(saveprint){
        dev.off()
        print(" saved")
      }
    }
  }
}
  
  


