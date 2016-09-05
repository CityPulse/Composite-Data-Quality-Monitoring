#install.packages(c('rgdal','rgeos','lattice','scales','leaflet','shiny','RColorBrewer','ggplot2','rjson'))
#install.packages('plyr')
library(plyr)
#install.packages('dplyr')
library(dplyr)
#install.packages('RCurl')
library(RCurl)
#install.packages('sp')
library(sp)
library(rgeos)
#install.packages('maptools')
library(maptools)
#install.packages('gdata')
library(gdata)
#install.packages('knitr')
library(knitr,quietly=T)
#install.packages('xtable')
#library(xtable)#Shiny
#install.packages('DT')
library(DT)
source('config.R')


histcol <- '#00ee00'
options(scipen=999)
crs=CRS("+proj=longlat +datum=WGS84") #EPSG:4360
crsS70=CRS("+proj=sterea +lat_0=46 +lon_0=25 +k=0.99975 +x_0=500000 +y_0=500000 +ellps=krass +units=m +no_defs ") #EPSG:31700

# local DataSet Stores
if(!exists("combined_datasets_data")) combined_datasets_data <<- list()
if(!exists("combined_datasets_metainformation")) combined_datasets_metainformation <<- list()
if(!exists("simple_datasets")) simple_datasets <<- list()
spatial_data_frames_loaded <<- data.frame(ds_name=character(), 
                                        id_column = character(),
                                        time_column = character(),
                                        value_column =  character(),
                                        fromDate=as.POSIXct(character()),
                                        toDate=as.POSIXct(character()),
                                        stringsAsFactors = FALSE) 

map <- NULL
spatialdataLines <- list()
spatialdataPoints <- list()
multiwkt <- list()
lastStreamPool <- list()
qoidatalist <- list()



nullToNA <- function(x) {
  x[x=='inf'] <- NA
  x[sapply(x, is.null)] <- NaN
  return(x)
}


print('Initialising GeoDbConnection')
source(file="initGeoDb.R")
print('--- OK')
print('Initialising GoogleRoutesConnection')
source(file="initGoogleRoutes.R")
print('--- OK')
print(length(simple_datasets))
if (length(simple_datasets)<1){
  print('Loading Test Datasets')
  source('loadTestDataSets.R') 
  print('--- OK')
} else {
  print("Keeping old test data")
}

#performance options
offline = FALSE
