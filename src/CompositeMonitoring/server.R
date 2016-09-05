library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(rgdal)
library(rgeos)
library(ggplot2)
library(rjson)

shinyServer(function(input, output, session) {  
  #updateBaseData()
  #print(input)
  ip<<-input
  # When map is clicked, show a popup with city info
  obsMap<-observe({
    print("ObserveMap")
    leafletProxy("map") %>% clearPopups()
    event_shape <- input$map_shape_click
    event_marker <- input$map_marker_click  
    
    if (!is.null(event_shape)){
      print('Shape: ')
      showQoiPopup(event_shape$id, event_shape$lat, event_shape$lng)
    }
    if (!is.null(event_marker)){
      print('Marker: ')
      showQoiPopup(event_marker$id, event_marker$lat, event_marker$lng)
    }
    # isolate({
    #   showQoiPopup(event$id, event$lat, event$lng)
    # })
  })
  
  showQoiPopup <- function(uuid, lat, lng) {
    print("Show PopUp")
    print(uuid)
    print(lat)
    print(lng)
    print("- - - -")
    print(spatial_data_frames_loaded)
    print("- - - -")
    content = ""
    for (l in 1:nrow(spatial_data_frames_loaded)){
      ds_name = spatial_data_frames_loaded[l,'ds_name']
      id_column = spatial_data_frames_loaded[l,'id_column']
      print(paste("-- ",ds_name, id_column))
      sds=simple_datasets[[ds_name]]@data
      rows = which(sds$id_column_cm == uuid)
      print(paste(--- rows ," datasets with id ",uuid, "at this location",lat, lng))
      content = paste(content, as.character(
        tagList(
          tags$h4("ds_name:", ds_name),
          tags$h5("IdColumn: ",id_column),
          tags$h5("UUID: ",uuid),
          tags$h5("Rows: ",rows)
          
          #HTML(kable(s.df, format='html', digits=2, pad=10))
          #HTML(print(xtab, type="html", include.rownames = TRUE,scalebox = 0.7, html.table.attributes = list('border="1" bgcolor="#FFCC00"')))
        )))
    }
    # content <- as.character(
    #   tagList(
    #     tags$h4("ds_name:", ds_name),
    #     tags$h5("IdColumn: ",id_column),
    #     tags$h5("UUID: ",uuid),
    #     tags$h5("sds: ",sds),
    #     tags$h5("Rows: ",rows)
    #     
    #     #HTML(kable(s.df, format='html', digits=2, pad=10))
    #     #HTML(print(xtab, type="html", include.rownames = TRUE,scalebox = 0.7, html.table.attributes = list('border="1" bgcolor="#FFCC00"')))
    #   )
    # )
    lastuuid<<-uuid
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = uuid)
  }
  
  

  
  ## Interactive Map ###########################################  
  # Create the map
  output$map <- renderLeaflet({
    print('RenderLeaflet')
    leaflet() %>%
      #addProviderTiles("CartoDB.Positron")%>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 10.21, lat = 56.1575, zoom = 14)
  })

  

  
  
  getRenderHistPlot <- function(parameterName, aggregation, analysis, maintitle=NULL){#Function to create histograms
    # if (aggregation=="current"){
    #   colorBy <- paste0(parameterName,".",aggregation,".rated")      
    # } else{
    #   colorBy <- paste0(parameterName,".",aggregation,".rated.",analysis)
    # }
    # print("!!!HIST")
    # #print(colorBy)
    # #print(colnames(qoiInBounds()))
    # data<-qoiInBounds()[,colorBy]
    # #print(nrow(data))
    # #histdebugdata <<- data
    # #print('!')
    # if (length(data) == 0)##SHOULDNT THIS BE AVOIDED?? Recalc!!!
    #   return(NULL)
    # qoiBreaks <- seq(0,1,0.1)
    # #par(mar=c(4,3,0,0)+0.1)
    # par(mar=c(4,5,0,0)+0.1)
    # h <- hist(as.numeric(data),
    #           breaks = qoiBreaks,
    #           #main = "QoI Score (all visible sensors)",
    #           main = maintitle,
    #           xlab = parameterName,
    #           ylab = "#",
    #           xlim = range(qoiBreaks),#range(data[,1]),
    #           col = histcol,
    #           border = 'white')
    BMI<-rnorm(n=1000, m=24.2, sd=2.2) 
    h<-hist(BMI)
    return(h) 
  }
  
  

  

  


  obsClearMap <- observe({
    print("Map: Clear")
    button <- input$clear_map_button
    print(paste("clear_map_button:",button))
    if(button>0){
        map <<- leafletProxy("map", deferUntilFlush=FALSE) %>%clearShapes() %>%clearMarkers() %>% clearShapes()
      print('-- cleared')
    }
  })
  
  obsSimpleDf <- observe({
    print("Observer: Simple Dataframe Selected and should now be displayed in the map")
    button <- input$simple_data_button
    
    print(paste("simple_data_button:",button))
    if(button>0){
      simple <- isolate(input$simple_select)
      timing_to_from <- isolate(input$dateSelect)
      print(paste("-- " , simple))
      print(paste("-- " , colnames(simple_datasets[[simple]])))
      print(paste("-- " , timing_to_from))
      df <- subset(simple_datasets[[simple]],
                   as.Date(simple_datasets[[simple]]$import_time_stamp)<=timing_to_from[2]&
                     as.Date(simple_datasets[[simple]]$import_time_stamp)>=timing_to_from[1])
      print(paste("-- " , class(df)[[1]], nrow(df)))
      cnames = colnames(.GlobalEnv$spatial_data_frames_loaded)
      loc_select_id <- isolate(input$simple_select_id)
      #spatial_data_frames_loaded <<- rbind(c(simple,input$simple_select_id,input$simple_select_time,input$simple_select_value,timing_to_from[1],timing_to_from[2]),spatial_data_frames_loaded)
      isolate(
        newline <- list(simple,loc_select_id,input$simple_select_time,input$simple_select_value,as.POSIXct(min(simple_datasets[[simple]]$import_time_stamp)),as.POSIXct(max(simple_datasets[[simple]]$import_time_stamp)))
      )
      
      print(spatial_data_frames_loaded)
      print("Vorher:")
      print(newline)
      print("Nachher:")
      spatial_data_frames_loaded[nrow(spatial_data_frames_loaded)+1,]<<-newline 
      print(spatial_data_frames_loaded)
      
      #colnames(.GlobalEnv$spatial_data_frames_loaded) = cnames
      
      spatial_printed_last <<- df ##debug
      #uuidname=paste0('df$',input$simple_select_id)
      #layerIds=df[,input$simple_select_id]
      
      print('debug')
      print(colnames(df@data))
      print(loc_select_id)
      df@data[,'id_column_cm']=df@data[,loc_select_id]
      summary(df)
      print("das war der summary")
      if(class(df)[[1]]=="SpatialPointsDataFrame"){
        print("-- Points")
       
        map <- leafletProxy("map", deferUntilFlush=FALSE) %>%
          #addCircleMarkers(data = df, radius=10, stroke=FALSE, fillOpacity=0.4, fillColor='black') 
        addCircleMarkers(data = df, radius=10,  layerId= ~id_column_cm ,stroke=FALSE, fillOpacity=0.4, fillColor='black') 
        #addMarkers(data = df) 
      }else if(class(df)[[1]]=="SpatialLinesDataFrame"){
        print("-- Lines")
        print(colnames(df@data))
        map <- leafletProxy("map", deferUntilFlush=FALSE) %>%
         addPolylines(data = df,layerId= ~id_column_cm)
        
        #addPolylines(data = df,layerId=input$simple_select_id)
        
      }else{
        print("-- Unsupported Geometry")
      }
      print('-- added')
    }
    output$shown_layer_table <- DT::renderDataTable(DT::datatable({
      spatial_data_frames_loaded
    },options = list(lengthMenu = c(), pageLength = -1,paging=FALSE,searching=FALSE, selection='single')))
    
  })
  #df <- simple_datasets[['tomtom_icons']][(as.Date(simple_datasets[['tomtom_icons']]$import_time_stamp)<=timingd[2])&(as.Date(simple_datasets[['tomtom_icons']]$import_time_stamp)>=timingd[1]),]
  #simple_datasets[[1]][(as.Date(simple_datasets[[1]]$import_time_stamp)<timingd[1]&as.Date(simple_datasets[[1]]$import_time_stamp)>timingd[2]),]
  

  
  
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  obsColor<-observe({
    print("Observe Color") 
    # qoimetric <- input$qoimetric
    # aggregation <- input$aggregation
    # analysis <- input$analysis
    # if (aggregation=="current"){
    #   colorBy <- paste0(qoimetric,".",aggregation,".rated")      
    # } else{
    #   colorBy <- paste0(qoimetric,".",aggregation,".rated.",analysis)
    # }
    # #print(colorBy)
    # event <- input$nav ##has to b here to reload
    # print("Updating Live Date")
    # 
    # #TODO
    # 
    # #Color
    # 
    # debugqoiuinbounds <<- qoiInBounds()
    # 
    # print("Subsetting Data")
    # #print(colnames(qoiInBounds()))   
    # #print(paste("Looking for Column", colorBy))
    # datasubset <- qoiInBounds()[c('longitude','latitude','uuid', colorBy)]
    # #print("--- QoI in Bounds")
    # #print(nrow(datasubset))
    # #print(colnames(datasubset))
    # #print(datasubset)
    # print("--- OK(Subset)")
    # colnames(datasubset) <- c('longitude','latitude','uuid','val')
    # print('2b')
    # #print(colnames(datasubset))
    # print(nrow(datasubset))
    # #print(datasubset)
    # datasubset <- subset(datasubset, !is.na(val))
    # #print(nrow(datasubset))
    # 
    # aggreg <- subset(datasubset, !is.na(val)) # datasubset#aggregated in the interface
    # #     print(paste ("Aggreg  with ",inputCalculation))
    # #     if( inputCalculation=="window"){
    # #        #TODO
    # #        aggreg = ddply(datasubset,c('longitude','latitude','uuid'), summarize, val=sum(val))
    # #      } else if( inputCalculation=="mean"){
    # #        aggreg = ddply(datasubset, c('longitude','latitude','uuid'), summarize, val=mean(val))
    # #      }else if( inputCalculation=="median"){
    # #        aggreg = ddply(datasubset, c('longitude','latitude','uuid'), summarize, val=median(val))
    # #      }else if( inputCalculation=="minimum"){
    # #        aggreg = ddply(datasubset, c('longitude','latitude','uuid'), summarize, val=min(val))
    # #      }else if( inputCalculation=="maximum"){
    # #        aggreg = ddply(datasubset, c('longitude','latitude','uuid'), summarize, val=max(val))
    # #      }else if( inputCalculation=="tendency"){
    # #        #todo
    # #        aggreg = ddply(datasubset, c('longitude','latitude','uuid'), summarize, val=sum(val))
    # #      }
    # #      print("--- OK(Aggreg)")
    # #     
    # 
    # print("Coloring")
    # aggreg[['val']] <- as.numeric(aggreg[['val']])
    # colorData <- aggreg[['val']]
    # 
    # if((length(unique(colorData))>1) && abs(max(colorData)-min(colorData))>0.01 && (input$colorscale!='fullscale')){
    #   pal <- colorBin(input$colorchooser, colorData, 7, pretty = FALSE)
    # }else{
    #   pal <- colorBin(input$colorchooser, c(1,0), 10, pretty = FALSE)#if we just have 1 color we cant do a scale
    # }
    # 
    # print(paste("Should now print ",nrow(aggreg)))
    # map <<- leafletProxy("map", data = aggreg, deferUntilFlush=FALSE) %>%
    #   clearShapes() %>% #clearLegends()%>%
    #   addCircles(~longitude, ~latitude,  radius=70, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   #addGeoJSON(geoj, stroke=TRUE, fill=FALSE, fillOpacity=0.4) %>%
    #   #addCircles(~longitude, ~latitude,  radius=70, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   #addCircleMarkers(~longitude, ~latitude, radius=10, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%         
    #   addLegend(position="bottomleft", pal=pal, values=colorData, title=colorBy, layerId="map")
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  obsAnno<-observe({
    # print("ObserveAnnotation") 
    # event <- input$nav ##has to b here to reload   
    # #datasubset <- qoiInBounds()[c('longitude','latitude','streamid', colorBy)]    
    # map2 <<- leafletProxy("map2",  deferUntilFlush=FALSE) %>%
    #   clearShapes() 
    # print("--- OK(ObserveAnnotation)")
  })
  
  obsRequestId<-observe({
    # request_id <- input$requestid ##has to b here to reload
    # print(paste("ObserveRequestId",request_id ))
    # if(length(input$requestid)>0){
    #   print('before')
    #   #geoj<-getRouteJson(con, request_id) %>% fromJSON(simplifyVector = FALSE)
    #   geojraw<-getRouteJson(con, request_id)
    #   #print(geojraw)
    #   geoj<-fromJSON(json_str=geojraw)#,   simplifyVector = FALSE)
    #   print('after')
    #   if(is.null(geoj$features)){
    #     print("Featurecount is NULL")
    #     return(NULL)
    #   }else{
    #     print(geoj$features)
    #   }
    #   geoj$style = list(
    #     weight = 3,
    #     color = "#555555",
    #     opacity = 1,
    #     fill = FALSE,
    #     stroke = TRUE
    #   )
    #   costs <- sapply(geoj$features, function(feat) { feat$properties$cost })
    #   routeid <- sapply(geoj$features, function(feat) { feat$properties$route_id })
    #   if(length(costs)==1) {
    #     costs=c(costs-1,costs,costs+1)
    #   }
    #   print(input$colorchooser);
    # 
    #   pal <- colorBin(input$colorchooser, costs, 7, pretty = FALSE)#colorQuantile("Greens", costs)
    #   geoj$features <- lapply(geoj$features, function(feat) {
    #     feat$properties$style <- list(
    #       color = pal(feat$properties$cost)
    #     )
    #     feat
    #   })
    #   from_x <- request_ids_geos$from_x[request_ids_geos$request_id==request_id]
    #   from_y <- request_ids_geos$from_y[request_ids_geos$request_id==request_id]
    #   to_x <- request_ids_geos$to_x[request_ids_geos$request_id==request_id]
    #   to_y <- request_ids_geos$to_y[request_ids_geos$request_id==request_id]
    #   #datasubset <- qoiInBounds()[c('longitude','latitude','streamid', colorBy)]
    #   map3 <<- leafletProxy("map3",  deferUntilFlush=FALSE) %>%
    #     clearGeoJSON()%>% clearMarkers()%>% #clearLegends()%>%
    #     addGeoJSON(geoj)%>%
    #     #addGeoJSON(geoj, stroke=TRUE, fill=FALSE, fillOpacity=0.4, color="black")%>%
    #     addMarkers(lng=from_x, lat=from_y, popup="Start Position")%>%
    #     addMarkers(lng=to_x, lat=to_y, popup="Destination")%>%
    #     addLegend(position="bottomleft", pal=pal, values=costs, title='Cost', layerId="map3")
    #   #map3$style <- list(weight = 5, stroke = "true", fill = "true", opacity = 1, fillOpacity = 0.4)
    # }
  })

  
  
  

  
  
  #Performance
  options = reactive({
    input$category
  })
  
  offlineMode = reactive({
    input$offlineMode
  })
  

  # output$info <- renderUI({
  #   tryCatch({
  #     wData <- getWrapperInformation(input$uuid)
  #     HTML(paste(wData$sensorName, wData$sensorType[1], wData$streetName[1], paste(wData$postalCode[1], wData$cityName[1], sep=" "), sep=" <br/>"))
  #   },
  #   error=function(cond){
  #     HTML(paste("NA", sep="<br/>"))
  #   })
  # })
  
  #Output Table simple Data
  output$simple_table <- DT::renderDataTable(DT::datatable({
    select = input$simple_select
    #print("x1")
    #print(select)
    data = simple_datasets[[select]]
    #print(class(data))
    #print(summary(data))
    data@data
  }))
  
  #Output Tables Joined Data
  output$combined_datasets_data_table <- DT::renderDataTable(DT::datatable({#will this allways be a spatial df?
    print("Select Combined Data")
    print(input$combined_select_data)
    rows=min(max_rows_table,nrow(combined_datasets_data[[input$combined_select_data]]))
    data<-NULL
    if(class(combined_datasets_data[[input$combined_select_data]])=='data.frame'){
      data <- combined_datasets_data[[input$combined_select_data]][1:rows,]
    }else(
      data <- combined_datasets_data[[input$combined_select_data]]@data[1:rows,]
    )
    data
  },options = list(lengthMenu = c(5,10, 30, 50), pageLength = 5)))
  #Output Tables Joined Metadata
  output$combined_datasets_metainformation <- DT::renderDataTable(DT::datatable({ #will this allways be a spatial df?
    print("Select Combined Meta Table Update")
    print(input$combined_select_meta)
    rows=min(max_rows_table,nrow(combined_datasets_metainformation[[input$combined_select_meta]]))
    data<-NULL
    if(class(combined_datasets_metainformation[[input$combined_select_meta]])=='data.frame'){
      data<-combined_datasets_metainformation[[input$combined_select_meta]][1:rows,]
    }else{
      data<-combined_datasets_metainformation[[input$combined_select_meta]]@data[1:rows,]
    }
    data
  },options = list(lengthMenu = c(5,10, 30, 50), pageLength = 5)))
  
  # Update Column Names of Simple Dataset Selection
  observe({
    print("Observe: Simple DS Change")
    cols <- colnames(simple_datasets[[input$simple_select]]@data)
    classes <- sapply(simple_datasets[[input$simple_select]]@data,class)
    
    #which one is a POSIXct Class? classes <- lapply(simple_datasets[[1]]@data,class)
    lapply(classes,length)
    date_col_id = which(unlist(lapply(classes,function(x) x[1]=='POSIXct')))
    print(paste('--  fields: ',colnames(simple_datasets[[input$simple_select]])))
    print(paste('-- selected date field: ',date_col_id))
    print(paste('-- selected date field: ',colnames(simple_datasets[[input$simple_select]]@data)[date_col_id]))
    print(paste("-- ",cols,sep=" ",collapse = " "))
    tryCatch({
     # which(classes=="POSIXct")
      
      #classes[2][[1]][[1]]
      updateSelectInput(session, inputId = "simple_select_id", choices = cols)
      updateSelectInput(session, inputId = "simple_select_time", choices = cols,selected = colnames(simple_datasets[[input$simple_select]]@data)[date_col_id])
      updateSelectInput(session, inputId = "simple_select_value", choices = cols)
      print("-- updated")
    },
    error=function(cond){
      updateSelectInput(session, inputId = "simple_select_id", choices = "NA")
      updateSelectInput(session, inputId = "simple_select_time", choices = "NA")
      updateSelectInput(session, inputId = "simple_select_value", choices = "NA")
      message(paste("error: ",cond))
    })
  })
  
  observe({
    ds <- input$simple_select_time
    time_column <- input$simple_select_time
    tryCatch({
    updateDateRangeInput(session,"dateSelect",
                           start = min(simple_datasets[[ds]]@data[,time_column]), end = max(simple_datasets[[ds]]@data[,time_column]), 
                           min = min(simple_datasets[[ds]]@data[,time_column]), max = max(simple_datasets[[ds]]@data[,time_column]))
    },
    error=function(cond){
      #message(paste("error: ",cond))
    })
  })

  
  # Update Column Names
  observe({
    cols <- colnames(combined_datasets_data[[input$combined_select_data]])
    tryCatch({
      updateSelectInput(session, inputId = "combined_join_data", choices = cols)
    },
    error=function(cond){
      updateSelectInput(session, inputId = "combined_join_data", choices = "NA")
      message(paste("error: ",cond))
    })
  })
  # Update Column Names
  observe({
    cols <- colnames(combined_datasets_metainformation[[input$combined_select_meta]]@data)
    print("Select Combined Meta Column Update")
    print(input$combined_select_meta)
    tryCatch({
      updateSelectInput(session, inputId = "combined_join_meta", choices = cols)
      #print("s1")
      #print (names(combined_datasets_metainformation))
      #print (input$combined_select_meta)
      #print (cols)
      #print("s2")
    },
    error=function(cond){
      updateSelectInput(session, inputId = "combined_join_meta", choices = "NAx")
      message(paste("error: ",cond))
    })
  })
    
  # Observer if ID Columns match and trigger button availability for joining the data
  observe({
    tryCatch({
      df_meta <- input$combined_select_meta
      df_data <- input$combined_select_data
      col_meta <- input$combined_join_meta
      col_data <- input$combined_join_data
      #print(paste(df_meta,df_data,col_meta,col_data))
      fields_data <<- unique(combined_datasets_data[[df_data]][,col_data])
      fields_meta <<- unique(combined_datasets_metainformation[[df_meta]]@data[,col_meta])
      #print(fields_data)
      #print(fields_meta)
      matching_ids=is.element(fields_data,fields_meta)
      if(length(matching_ids[matching_ids==FALSE])==0){
       print('All IDs match') 
        updateButton(session, inputId="combine_data_button", disabled=FALSE,icon = icon("plus"))
      }else{
       print('Found unmatched IDs') 
        updateButton(session, inputId="combine_data_button", disabled=TRUE,icon = icon("ban"))
      }
      #print(paste('Combined dataset matches join: ',is.element(fields_data,fields_meta)))
    },
    error=function(cond){
      #message(paste("error: ",cond))
      #print('Combined datasets do not match join')
    })
  })
  
  #Combine Metadtata and Date and ad to simple DS
  observe({
    press <- input$combine_data_button
    print(paste("combine_data_button: ", press))
    if(press>0){
      df_meta <- input$combined_select_meta
      df_data <- input$combined_select_data
      col_meta <- input$combined_join_meta
      col_data <- input$combined_join_data
      #print(summary(combined_datasets_metainformation[[df_meta]]))
      #sp@data = data.frame(sp@data, df[match(sp@data[,by], df[,by]),]) # thanks http://stackoverflow.com/questions/3650636/how-to-attach-a-simple-data-frame-to-a-spatialpolygondataframe-in-r
      combined_datasets_metainformation[[df_meta]]@data <<- data.frame(combined_datasets_metainformation[[df_meta]]@data, combined_datasets_data[[df_data]][match(combined_datasets_metainformation[[df_meta]]@data[,col_meta], combined_datasets_data[[df_data]][,col_data]),])
      #print(summary(combined_datasets_metainformation[[df_meta]]))
      simple_datasets[[paste0(df_meta,'/',df_data)]] <<- combined_datasets_metainformation[[df_meta]]
      print("Added to simple DS")
      print(names(simple_datasets))
      updateSelectInput(session, inputId = "simple_select", choices = names(simple_datasets))
    }
  })
  

})



