library(scatterplot3d)
library(ggplot2)
library(sp)
library(rgeos)
setwd("~/svn/CityPulseDevelopment/trunk/A4.3/CorrelationMonitoring/CorrelationSystem")


if (exists('con')){
  dbDisconnect(con)
  #dbClearResult(dbListResults(con)[[1]])
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="xxxx", dbname="cp_traffic", port=5435, host="localhost")

library(rgdal)
mydsn="PG:dbname='cp_traffic'  host=localhost user=citypulse password=xxxx port=5435"
#ogrListLayers(mydsn)
#ogrInfo(mydsn, "ways")
ways = readOGR(dsn=mydsn,layer="ways", verbose = TRUE,stringsAsFactors=FALSE)
forti_ways = fortify(ways)
#ggplot(data=forti_ways, aes(long, lat, group=group)) + 
#  geom_polygon(colour='black',fill='white') +theme_bw()



p  <- readRDS("/Users/danielk/experiments/results_try_all_with_all_data_2016-06-27.RDS") 

interesting_speed=c(7645, 7588,3153,2045)
paperExample=
min_max_timegraph = c(1464297500, 1464299500) 
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
    for (j in 1:length(d_list)){
        #report_df <- data.frame(d_list[[j]]$time_stamp,d_list[[j]]$vehicleCount_raw)
        report_df <- data.frame(d_list[[j]]$time_stamp,d_list[[j]]$averageSpeed_random_dct)[6:1,]
        colnames(report_df) <- c('date','val')
        report_df['origin'] <- paste0('ODAA(',ids[j],')')
        wkt = readWKT(location_p1[j])
        report_df['lon'] <- wkt$x
        report_df['lat'] <- wkt$y
        colnames(report_df)
        result.df <- rbind(result.df,report_df[c("lat","lon", "date",   "val"  ,  "origin"   )])
    }
      lat <- as.numeric(p[[i]]$icon_coord[2])
      lon <- as.numeric(p[[i]]$icon_coord[1])
      val <- p[[i]]$severity*(-1)
      date <- p[[i]]$icon_data[['import_time_stamp']]
      origin <- paste0('ODAA(',p[[i]]$icon_data[['poi_id']],')')
      event.df <- data.frame(lat,lon,date,val,origin)
      result.df <- rbind(result.df,event.df)
      result.df$pcolor <- "black"
      #legend_seq = seq(-10,20,length.out=3,along.with=0.0)
      
      result.df <- result.df[c(1:6,13:18,25:42,19:24),]
      result.df$pcolor[result.df$val > 0] <- "blue"
      result.df$pcolor[result.df$val <=0 ] <- "green"
      result.df$pcolor[result.df$val <(-5)] <- "orange"
      result.df$pcolor[result.df$val < (-10)] <- "violet"
      
      

      min_lat <- min(result.df$lat)
      min_lon <- min(result.df$lon)
      max_lat <- max(result.df$lat)
      max_lon <- max(result.df$lon)
      min_lat <- 56.176
      min_lon <- 10.192
      max_lat <- 56.186
      max_lon <- 10.202
      delta_lat = (max_lat-min_lat)*0.05
      delta_lon = (max_lon-min_lon)*0.05
      fortiselect <- forti_ways[forti_ways$long > min_lon-delta_lon & forti_ways$long < max_lon+delta_lon & forti_ways$lat > min_lat-delta_lat & forti_ways$lat < max_lat+delta_lat,]
      #fortiselect <- forti_ways[forti_ways$long > min_lon & forti_ways$long < max_lon & forti_ways$lat > min_lat & forti_ways$lat < max_lat,]
      
      #Z-Min DATE
      fortiselect$date=min(result.df$date)
      fortiselect$date=1464297500
      fortiselect$group <- as.numeric(fortiselect$group)
      # min_time=min(result.df$date)
      # max_time=max(result.df$date)
      # time_2 = min_time+((max_time-min_time)/3)
      # time_3 = min_time+((max_time-min_time)/3*2)
      # times = c(min_time,time_2,time_3,max_time)
      time_1 =as.POSIXct(1464297500, origin = "1970-01-01",tz = "UTC")
      time_2 =as.POSIXct(1464298000, origin = "1970-01-01",tz = "UTC")
      time_3 =as.POSIXct(1464298500, origin = "1970-01-01",tz = "UTC")
      time_4 =as.POSIXct(1464299000, origin = "1970-01-01",tz = "UTC")
      time_5 =as.POSIXct(1464299500, origin = "1970-01-01",tz = "UTC")
      times = c(time_1,time_2,time_3,time_4,time_5)
      
      strftime(times, format="%H:%M")
      z_ticks = c(min(result.df$date),max(result.df$date))
      
      
      aspect_ratio = delta_lon/delta_lat
      
      pdf(paste0('results/Paper_Speed_3dt_',i,'(',length(d_list),')_v3.pdf'),width = 9, height = 8)
      #dev.off()
      
      s3d <- scatterplot3d(event.df$lat,
                           event.df$lon,
                           event.df$date,      # x y and z axis
                           color='white', pch=19,        # circle color indicates no. of cylinders
                           type="p", 
                           #main="Traffic Time Flow",
                           xlab="Longitude",
                           ylab="",
                           zlab="Time",
                           angle=45,
                           scale.x = aspect_ratio,
                           z.ticklabs = strftime(times, format="%H:%M"),
                           col.axis = grey(.4),
                           col.grid = "white",
                           cex.axis = .9,
                           xlim = c(as.numeric(min(result.df$lat)),as.numeric(max(result.df$lat))),
                           ylim = c(as.numeric(min(result.df$lon)),as.numeric(max(result.df$lon))),
                           zlim =c(as.numeric(min(result.df$date)),as.numeric(max(result.df$date)))
      )
      dims <- par("usr")
      x <- dims[1]+ 0.9*diff(dims[1:2])
      y <- dims[3]+ 0.08*diff(dims[3:4])
      text(x-.5,y+.3,expression(Latitude),srt=45)
      
      groupids <- unique(fortiselect$group)
      for (g in groupids){
        forti_sub_select <- fortiselect[fortiselect$group==g,]
        s3d$points3d(forti_sub_select$lat,
                     forti_sub_select$long,
                     forti_sub_select$date,      # x y and z axis
                     col=grey(.5), type='l'
        )
      }
      s3d$points3d(result.df$lat,
                   result.df$lon,
                   result.df$date,      # x y and z axis
                   col=result.df$pcolor, pch=19,        # circle color indicates no. of cylinders
                   type="h" #,lty.hplot=2    
      )
      s3d.coords <- s3d$xyz.convert(result.df$lat, result.df$lon, result.df$date)
      text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
           labels=as.character(round(result.df$val,2)),       # text to plot
           pos=c(4,4,4,4,4,4,
                 4,4,4,4,4,4,
                 4,4,4,4,4,4,
                 4,4,4,4,4,4,
                 2,2,2,2,2,2,
                 2,2,2,2,2,2), cex=.8)                  # shrink text 50% and place to right of points)
      
      s3d.coords_event <- s3d$xyz.convert(event.df$lat, event.df$lon, event.df$date)
      
      text(s3d.coords_event$x, s3d.coords_event$y,
           labels=c("Severity 3"),       # text to plot
           pos=c(4), cex=.8, col='red') 
      
      # add the legend
      legend("topleft", inset=.01,      # location and inset
             bty="n", cex=.9,              # suppress legend box, shrink text 50%
             title="ODAA Speed\nChange (Irregular)",pch=c(16,16,16,16,8),
             c(">=  0","<   0", "<  -5", "< -10", "TomTom\nIncident"), col=c("blue","green","orange","violet","red"))
      

      s3d$plane3d(c(as.numeric(event.df$date),0,0),col='red', lty='dotted')
      s3d$points3d(event.df$lat,
                   event.df$lon,
                   event.df$date,      # x y and z axis
                    pch=8,col='red')
      
     dev.off()
      
}

