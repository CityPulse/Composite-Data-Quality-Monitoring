#GoogleToolsInit

#FindGoogleRoute
#install.packages('RPostgreSQL')  
#install.packages('rgeos')
#install.packages('rjson') #alternativ apt-get install r-cran-json
library(rgeos)
library(rjson)
library(rgdal)
#install.packages('V8')
library(V8)
library(sp)


getGoogleRouteLink <- function(p1_src_geo, p2_src_geo){
  request = paste("http://maps.googleapis.com/maps/api/directions/json?origin=",p1_src_geo$y,",",p1_src_geo$x,"&destination=",p2_src_geo$y,",",p2_src_geo$x,"&sensor=false&mode=driving", sep="")
  return (request)
}

getGoogleRouteSPDF <- function(p1,p2,id){
  googleJsonQuery = getGoogleRoute(p1, p2)
  print(googleJsonQuery)
  docfilename = paste("tmp/",id,".json",sep="")
  print(docfilename)
  download.file(googleJsonQuery, destfile=docfilename) #we want to keep it for further eval
  document <- fromJSON(file=docfilename, method='C')
  routelist[[as.character(src_id)]]=document
  ctx <- new_context()
  ctx$source("polyline.js")
  keylist=data.frame(as.numeric(names(routelist)))
  colnames(keylist)=c('report_id')
  linelist=list()
  for(i in 1:length(routelist)){
    print(keylist[i,'report_id'])
    rl1= routelist[[i]]$routes
    rl1[[1]]$overview_polyline$points
    line = Line(data.frame(ctx$call("polyline.decode", rl1[[1]]$overview_polyline$points))[,c('X2','X1')])
    linelist[i]=Lines(list(line),ID=keylist[i,'report_id'])
  }
  splines = SpatialLines(linelist)
  plot(splines)
  rownames(keylist)=keylist[,'report_id']
  spldf = SpatialLinesDataFrame(splines,keylist,match.ID=TRUE)
  proj4string(spldf) <-CRS("+init=epsg:4326")
  return(spldf)
}


