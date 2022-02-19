library(dplyr)

server= function(input, output, session){

  
  # Introduction ------------------------------------------------------------
  
  
  
  query_modal <- modalDialog(
    title = "Analyze Discharge Data with Dischanalyst",
    "This App uses the GRDC-Dataset and the dischanalyst R-Package to analyze Discharge Measurements in Germany.
      If you have any Questions concering 
      the app's operation, please check the User's Guide. ",
    
    easyClose = F,
    footer = tagList(
      actionButton("start_window", "Explore")
    )
  )
  
  # Show the model on start up ...
  showModal(query_modal)
  
  observeEvent(input$start_window, {
    removeModal()
  })
  
  
  
  # First Page --------------------------------------------------------------
  
  
  
  
  # Map  ---------------------------------------------------------------------
  
  
  map = createLeafletMap(session, 'map')
  
  
  
  
  
  
  
  
  
  session$onFlushed(once = T, function() {
    
    
    getColor <- function(data) {
      sapply(data$rep_stat, function(rep_stat) {
        if(rep_stat==F) {
          "green"
          
        } else {
          "orange"
        } })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(data)
    )
    
    
    
    output$map <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addAwesomeMarkers(lat = ~latitude, lng = ~longitude, icon=icons,
                          clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T), 
                          
                          
                          popup = ~paste(
                            paste('<b>', 'River', '</b>', river), 
                            paste('<b>',  'Station', '</b>', station),
                            paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                            
                            sep = '<br/>'),
                          popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        addLegend("topright", colors = c("orange","green"), values = c("orange", "green"),labels = c("representative Station", "Station"),
                  title = "Legend",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        )%>% 
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        ) 
    })
  })
  
  
  # Table ---------------------------------------------------------------
  
  
  output$table_input=DT::renderDataTable({
    DT::datatable(data, selection='single', rownames=FALSE,colnames= c("grdc_no"="grdc_no" , "River"=   "river"   ,"Station"=   "station", "Country"=    "country", 
                                                                       "Catchment"="catch_area", "Altitude"= "altitude" , "Startyear"= "startyear" , "Endyear"= "endyear" ,
                                                                       "Time span"=   "d_years"   , "Longitude"= "longitude"  , "Latitude"="latitude"   ), filter="top",
                  options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr")
                  
                  
    )
  })
  
  
  
  
  # reactive Values
  
  # Help Button -------------------------------------------------------------
  
  
  #Help Button
  
  observeEvent(input$help,{
    showModal(modalDialog(
      title = "Need help?",
      "Please choose your settings. Afterwards select a station on the map or in the table. Calculating Trends may take some time. To choose new settings or a different station please select the Clear Data- Button before adapting the settings.",
      
    ))
  })
  
  
  
  # Empty functions ---------------------------------------------------------
  
  
  #Initial conditions: 'Select station on map.'
  t_plot <- function(){
    
   tpl= plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    return(tpl)
  }
  
  
  
  
  
  
  output$disch_plot=renderPlot({t_plot()})
  output$disch_plot=renderPlot({empty()})

  

  
  empty=   function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    return(plot)
    
  }
  
  output$disch_plot <- renderPlot({empty()})
  
  
  trendpl= function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station. ", line = -1, cex = 1.5)
    return(plot)
    
    
  }
  
  
selpl=   function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext(paste("Station:", stat_name, "selected"), line = -1, cex = 1.5)
    return(plot)
    
  }
  
  
  
  
  
  
  
  output$trendplot=renderPlot({trendpl()})
  
  
  
  thres= function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station. ", line = -1, cex = 1.5)
    return(plot)
    
    
  }
  
  output$thresplot= renderPlot({thres()})
  
  
  # Reactive Map -----------------------------------------------------
  
  
  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")
  
  #Reaction to selection of station on map
  observeEvent(input$map_marker_click,{
    
    gauge_sel$clicked_gauge <- input$map_marker_click
    
    stat_sel <- which(data$latitude == gauge_sel$clicked_gauge$lat)
    
    stat_name <- data$station[stat_sel] #station name
    
    
    
    #read discharge time series
    disc_data <- data2[[stat_name]]
    
    observe({
      
      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))
      
      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))-1
      
      updateSliderInput(session, "year", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      updateSliderInput(session, "year2", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      
      updateSliderInput(session, "yearq", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      updateSliderInput(session, "yearv", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      
      
    })
    
    observe({
      
      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))
      
      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))
      
      rast_time_init <- c(sta_yea_cla, end_yea_cla)
      
      
      updateNumericInput(session, "ssy", label = "Select Startyear:", sta_yea_cla, 
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      updateNumericInput(session, "sey", label = "Select Endyear:", sta_yea_cla+1, 
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      
    })
    
    
    
    t_plot <- function(){
      
      
      if(input$qplot_variety == "Discharge Plot"){
        
        if (input$pettitt1){
          Qplot=Qplot(data2, stat_name, T)
        }else{
        
        Qplot=Qplot(data2, stat_name, F)
        }
        return(Qplot)

      }
      if(input$qplot_variety == "annual Discharge Plot"){
        
        if (input$hyeardis){
          Year=input$year2
          
          if (input$pettitt2){
          qploty=Qploty(data2, stat_name, year=Year,h=T, pettitt=T)
          }else{  qploty=Qploty(data2, stat_name, year=Year,h=T, pettitt=F)    }
        }else{
          Year=input$year2
        if (input$pettitt2){
          qploty=Qploty(data2, stat_name, year=Year,h=F, pettitt=T)
        }else{  qploty=Qploty(data2, stat_name, year=Year,h=F, pettitt=F)    }
          
        }
       
        return(qploty)
        
      }
      if(input$qplot_variety == "annual Discharge Boxplot"){
        
        
        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)
        
      }
      if(input$qplot_variety == "Discharge Boxplot"){
        
        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)
        
      }
      
      
    }
    
    output$disch_plot <- renderPlot({t_plot()})
    
    
    if(input$qplot_variety == "Seasonplot"){
      
      observeEvent(input$printplot, {
        Startyear=input$ssy
        Endyear=input$sey
        month_start=input$season1
        month_end=input$season2
        
        seasonplot=seasonpl(data=data2, station=stat_name, Startyear=Startyear, Endyear=Endyear, month_start=month_start, month_end =month_end )
        
        output$disch_plot <- renderPlot({seasonplot})
      })

      
    }
    
    
  #  trendpl=function(){
      
        #"season_trend", "Choose Season", c("Year", "Winter", "Spring", "Summer", "Autumn"))
        
        observeEvent(input$season_trend,{
          if (input$season_trend=="Year"){
            season="Y"
          }
          if (input$season_trend=="Autumn"){
            season="AU"
          }
          if (input$season_trend=="Winter"){
            season="WI"
          }
          if (input$season_trend=="Spring"){
            season="SP"
          }
          if (input$season_trend=="Summer"){
            season="SU"
          }
          
          
          
          observeEvent(input$season_trend_2,{
            if (input$season_trend_2=="Year"){
              seas="Y"
            }
            if (input$season_trend_2=="Autumn"){
              seas="AU"
            }
            if (input$season_trend_2=="Winter"){
              seas="WI"
            }
            if (input$season_trend_2=="Spring"){
              seas="SP"
            }
            if (input$season_trend_2=="Summer"){
              seas="SU"
            }
      
          
       
          

        trendpl=function(){
          if (input$trendpltype=="Trend of minimum Values"){
            
            plotr=Qmin_trend(data=data2,  station=stat_name, mod=1) 
            return(plotr)
          }
          if (input$trendpltype=="NMxQ-Trend"){
            x_val=input$xVALUE
            
            plotr=NMxQ_trend(data=data2,  station=stat_name, x=x_val, seasonal=season, graphic=T)
            return(plotr)
          }
          if (input$trendpltype=="Trend of Mean Values"){
            
            
            plotr=MQ_trend(data=data2,  station=stat_name, seasonal=seas )
            return(plotr)
          }
          
          
          
        }
            
     
            output$trendplot=renderPlot({trendpl()})
            
     
          })
        
        
          })
        
        

      
    
  
    
    
    
    thres= function(){
      if(input$thres_type=="Quantile Based"){
        
        quantile=input$quantile
        Year=input$yearq
        
        qperipl=periodplot_quantile(data2, stat_name , quantile, year=Year, graph=T)
        return( qperipl)
      }
      if(input$thres_type=="Choose individual Value"){
        
        Val=input$value
        Year=input$yearv
        
        uperipl=U_periodploty(data2, stat_name ,U= Val, year=Year, graph=T)
        return( uperipl)
      }
      
      
      
    }
    output$thresplot=renderPlot({thres()})
    
    
    
    
    
  }) #Observe Event Map/Marker/Table Marker Click finishes 
  
  
  
  observeEvent(input$cleardata, {
    output$disch_plot=renderPlot({empty()})
  })
  
  
  observeEvent(input$cleardata2, {
    output$trendplot=renderPlot({empty()})
  })
  
  
  
  observeEvent(input$cleardata3, {
    output$thresplot=renderPlot({empty()})
  })
  
  
  
  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")
  
  
  # Reactive Table ----------------------------------------------------------
  
  
  observeEvent(input$table_input_row_last_clicked,{
    s= input$table_input_row_last_clicked
    
    #gauge_sel$clicked_gauge <- input$tableId_row_last_clicked
    
    
    #stat_sel <- which(data$latitude == gauge_sel$clicked_gauge$lat)
    
    stat_name <- data$station[s] #station name
    
    
    
    #read discharge time series
    
    
    
    
    
    disc_data <- data2[[stat_name]]
    
    observe({
      
      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))
      
      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))-1
      
      updateSliderInput(session, "year", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      updateSliderInput(session, "year2", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      
      
      updateSliderInput(session, "yearq", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      updateSliderInput(session, "yearv", label = "Select Year:",
                        min = sta_yea_cla, max = end_yea_cla)
      
      
      
    })
    
    observe({
      
      sta_yea_cla <- as.numeric(format(disc_data[1,1], "%Y"))
      
      end_yea_cla <- as.numeric(format(disc_data[nrow(disc_data),1], "%Y"))
      
      rast_time_init <- c(sta_yea_cla, end_yea_cla)
      
      
      updateNumericInput(session, "ssy", label = "Select Startyear:", sta_yea_cla, 
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      updateNumericInput(session, "sey", label = "Select Endyear:", sta_yea_cla+1, 
                         min = sta_yea_cla+1, max = end_yea_cla-1)
      
    })
    
    
    t_plot <- function(){
      
      
      if(input$qplot_variety == "Discharge Plot"){
        
        Qplot=Qplot(data2, stat_name)
        return(Qplot)
        
      }
      if(input$qplot_variety == "annual Discharge Plot"){
        
        
        Year=input$year2
        qploty=Qploty(data2, stat_name, year=Year,h=T)
        return(qploty)
        
      }
      if(input$qplot_variety == "annual Discharge Boxplot"){
        
        
        Year=input$year
        qboxploty=QBoxploty(data=data2,  station=stat_name, year=Year, h=T)
        return(qboxploty)
        
      }
      if(input$qplot_variety == "Discharge Boxplot"){
        
        qboxplot=QBoxplot(data=data2,  station=stat_name)
        return(qboxplot)
        
      }
      
      
    }
    
    output$disch_plot <- renderPlot({t_plot()})
    
    
    if(input$qplot_variety == "Seasonplot"){
      
      observeEvent(input$printplot, {
        Startyear=input$ssy
        Endyear=input$sey
        month_start=input$season1
        month_end=input$season2
        
        seasonplot=seasonpl(data=data2, station=stat_name, Startyear=Startyear, Endyear=Endyear, month_start=month_start, month_end =month_end )
        
        
      })
      output$disch_plot <- renderPlot({seasonplot})
      
    }
    
    
    #  trendpl=function(){
    
    #"season_trend", "Choose Season", c("Year", "Winter", "Spring", "Summer", "Autumn"))
    
    observeEvent(input$season_trend,{
      if (input$season_trend=="Year"){
        season="Y"
      }
      if (input$season_trend=="Autumn"){
        season="AU"
      }
      if (input$season_trend=="Winter"){
        season="WI"
      }
      if (input$season_trend=="Spring"){
        season="SP"
      }
      if (input$season_trend=="Summer"){
        season="SU"
      }
      
      
      
      observeEvent(input$season_trend_2,{
        if (input$season_trend_2=="Year"){
          seas="Y"
        }
        if (input$season_trend_2=="Autumn"){
          seas="AU"
        }
        if (input$season_trend_2=="Winter"){
          seas="WI"
        }
        if (input$season_trend_2=="Spring"){
          seas="SP"
        }
        if (input$season_trend_2=="Summer"){
          seas="SU"
        }
        
        
        
        
        
        trendpl=function(){
          if (input$trendpltype=="Trend of minimum Values"){
            
            plotr=Qmin_trend(data=data2,  station=stat_name, mod=1) 
            return(plotr)
          }
          if (input$trendpltype=="NMxQ-Trend"){
            x_val=input$xVALUE
            
            plotr=NMxQ_trend(data=data2,  station=stat_name, x=x_val, seasonal=season, graphic=T)
            return(plotr)
          }
          if (input$trendpltype=="Trend of Mean Values"){
            
            
            plotr=MQ_trend(data=data2,  station=stat_name, seasonal=seas )
            return(plotr)
          }
          
          
          
        }
        
        
        output$trendplot=renderPlot({trendpl()})
        
        
      })
      
      
    })
    
    
    
    
    
    
    
    
    
    thres= function(){
      if(input$thres_type=="Quantile Based"){
        
        quantile=input$quantile
        Year=input$yearq
        
        qperipl=periodplot_quantile(data2, stat_name , quantile, year=Year, graph=T)
        return( qperipl)
      }
      if(input$thres_type=="Choose individual Value"){
        
        Val=input$value
        Year=input$yearv
        
        uperipl=U_periodploty(data2, stat_name ,U= Val, year=Year, graph=T)
        return( uperipl)
      }
      
      
      
    }
    output$thresplot=renderPlot({thres()})
    
    
    
    
    
    
  })#Observe Event Map/Marker/Table Marker Click finishes 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # second Page: Trend Analysis ---------------------------------------------
  
  
  # germany Map -------------------------------------------------------------
  
  map = createLeafletMap(session, 'datamap')
  
  
  
  
  
  
  
  
  
  session$onFlushed(once = T, function() {
    
    mapdata=data
    
    
    output$datamap <- renderLeaflet({
      leaflet(mapdata) %>%
        clearPopups() %>% 
        clearMarkers() %>%
        addTiles() %>%
        
        addCircleMarkers(lat = ~latitude, lng = ~longitude, 
                         
                         
                         
                         popup = ~paste(
                           paste('<b>', 'River', '</b>', river), 
                           paste('<b>',  'Station', '</b>', station),
                           paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                           
                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        ) 
    })
  })  
  
  
  
  
  
  observeEvent({input$trendtype2}, {
    if (input$trendtype2=="MQ - Mean Discharge Trend"){
      mapd=MQlist
      
    }
    if (input$trendtype2=="Trend Minimum Values"){
      mapd=mintrendlist
      
    }
    if (input$trendtype2=="NMxQ"){
      
      
      
      if (input$xval=="7"){
        mapd=NMxQlist7
        
      }
      if (input$xval=="14"){
        mapd=NMxQlist14
        
      }
      if (input$xval=="30"){
        mapd=NMxQlist30
        
      }
      if (input$xval=="60"){
        mapd=NMxQlist60
        
      }
      
      
    }
    if ( input$trendtype2=="Low Flow Period"){
      mapd=Periodmeta
    }
  
      
      
      observeEvent({input$timerange2}, {
        
        
        
        mapdata=mapd[[input$timerange2]]  
        
        
        
        
        
        
        
        
        
        leafletProxy("datamap",session, data=mapdata )%>%
          clearPopups() %>% 
          clearMarkers() %>%
          addTiles() %>%
          addCircleMarkers(data=mapdata , lat = ~latitude, lng = ~longitude, 
                           
                           
                           
                           popup = ~paste(
                  
                             paste('<b>',  'Station', '</b>', station),
                             
                             
                             sep = '<br/>'),
                           popupOptions = popupOptions(closeButton = FALSE)
          )  %>%    
          
          addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
          
          
          
          
          addLayersControl(
            baseGroups = c("Open Street Map", "Terrain Background"),
            position = "topright",
            options = layersControlOptions(collapsed = F)
          )
        
        
        
        observeEvent({input$dataset}, {
          
          
          if(input$dataset=="Representative Stations only"){
            
            l=  length(mapdata$station)
            
            iden=rep(F,l)
            for ( i in 1:l){
              iden[i]=is.element(mapdata$station[i], repres)
              
            }
            if(any(iden)==T){
              
              mapdata=mapdata[which(iden==T),] 
              
              
              
              
              
            }else {renderText("No Stations available")}
            
            
            leafletProxy("datamap",session, data=mapdata )%>%
              clearPopups() %>% 
              clearMarkers() %>%
              addTiles() %>%
              addCircleMarkers(data=mapdata ,lat = ~latitude, lng = ~longitude, 
                               
                               
                               
                               popup = ~paste(
                       
                                 paste('<b>',  'Station', '</b>', station),
                                 
                                 
                                 sep = '<br/>'),
                               popupOptions = popupOptions(closeButton = FALSE)
              )  %>%    
              
              addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
              addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
              
              
              
              
              addLayersControl(
                baseGroups = c("Open Street Map", "Terrain Background"),
                position = "topright",
                options = layersControlOptions(collapsed = F)
              )
            
            
            
            
            
            
            
          }else if(input$dataset=="All GRDC-Stations in Germany"){
            
            
            leafletProxy("datamap",session, data=mapdata )%>%
              clearPopups() %>% 
              clearMarkers() %>%
              addTiles() %>%
              addCircleMarkers(data=mapdata ,lat = ~latitude, lng = ~longitude, 
                               
                               
                               
                               popup = ~paste(
                            
                                 paste('<b>',  'Station', '</b>', station),
                                 
                                 
                                 sep = '<br/>'),
                               popupOptions = popupOptions(closeButton = FALSE)
              )  %>%    
              
              addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
              addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
              
              
              
              
              addLayersControl(
                baseGroups = c("Open Street Map", "Terrain Background"),
                position = "topright",
                options = layersControlOptions(collapsed = F)
              )
            
            
            
            
            
            
            
            
            
          }
          
          
        
        
      
          #season, color collected
        
          observeEvent({input$go}, {
            
            
            
            
            
            
            
            
            if(input$trendtypemq== "Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
              
              if (input$seasonmq=="Spring"){
                
                
                mapdata$Spslopezyp=cut(  mapdata$Spslopezyp, breaks=c(-2, -1, 0, 1, 2), 
                                         labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Spslopezyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Spslopezyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>', Spslopezyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
                
                
                
                
                
              }
              if (input$seasonmq=="Summer"){
                
                
                
                
                mapdata$Sslopezyp=cut(  mapdata$Sslopezyp, breaks=c(-2, -1, 0, 1, 2), 
                                        labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Sslopezyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Sslopezyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>', Sslopezyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
              }
              if (input$seasonmq=="Autumn"){
                
                
                mapdata$Aslopezyp=cut(  mapdata$Aslopezyp, breaks=c(-2, -1, 0, 1, 2), 
                                        labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Aslopezyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Aslopezyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',  Aslopezyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
              }
              if (input$seasonmq=="Winter"){
                
                
                mapdata$Wslopezyp=cut(  mapdata$Wslopezyp, breaks=c(-2, -1, 0, 1, 2), 
                                        labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Wslopezyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Wslopezyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Wslopezyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
              }
              if (input$seasonmq=="Year"){
                
                
                mapdata$Yslopezyp=cut(  mapdata$Yslopezyp, breaks=c(-2, -1, 0, 1, 2), 
                                        labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Yslopezyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Yslopezyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Yslopezyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
              }
              
              
              
              }
            
            
            
            if(input$trendtypemq== "Linear Model: Least Squares Approach"){
              
              if (input$seasonmq=="Spring"){
                
                
                mapdata$Spslopelm=cut(  mapdata$Spslopelm, breaks=c(-2, -1, 0, 1, 2), 
                                        labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Spslopelm)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Spslopelm), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Spslopelm), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
              }
              if (input$seasonmq=="Summer"){
                
                
                mapdata$Sslopelm=cut(  mapdata$Sslopelm, breaks=c(-2, -1, 0, 1, 2), 
                                       labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Sslopelm)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Sslopelm), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',  Sslopelm), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
              }
              if (input$seasonmq=="Autumn"){
                
                mapdata$Aslopelm=cut(  mapdata$Aslopelm, breaks=c(-2, -1, 0, 1, 2), 
                                       labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Aslopelm)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Aslopelm), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Aslopelm), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
              }
              if (input$seasonmq=="Winter"){
                
                mapdata$Wslopelm=cut(  mapdata$Wslopelm, breaks=c(-2, -1, 0, 1, 2), 
                                       labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Wslopelm)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Wslopelm), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',  Wslopelm), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
              }
              if (input$seasonmq=="Year"){
                
                mapdata$Yslopelm=cut(  mapdata$Yslopelm, breaks=c(-2, -1, 0, 1, 2), 
                                       labels=c("-1 - -2", "-1 - 0", "0 - 1", "1-2"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Yslopelm)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearMarkers() %>%
                  clearControls()%>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Yslopelm), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>', Yslopelm), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=c("-1 - -2", "-1 - 0", "0 - 1", "1-2") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
              }
              }
            
            #mapdata$Spsigzyp)
            if(input$trendtypemq== "Significance of Zyp-Trend"){
              
              if (input$seasonmq=="Spring"){
                
                
                mapdata$Spsigzyp=cut(  mapdata$Spsigzyp, breaks=c(-1, 0.2 ,0, 0.2,1), 
                                        labels=  c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Spsigzyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Spsigzyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',  Spsigzyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=       c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
              }
              if (input$seasonmq=="Summer"){
         
                
                mapdata$Ssigzyp=cut(  mapdata$Ssigzyp, breaks=c(-1, 0.2 ,0, 0.2,1), 
                                      labels=  c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Ssigzyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Ssigzyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Ssigzyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=       c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
              }
              if (input$seasonmq=="Autumn"){
                
                mapdata$Asigzyp=cut(  mapdata$Asigzyp, breaks=c(-1, 0.2 ,0, 0.2,1), 
                                    labels=  c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Asigzyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Asigzyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Asigzyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(title="Significance of Zyp-Trend in Autumn", position="topright", pal=COL,values=       c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
              }
              if (input$seasonmq=="Winter"){
                
                mapdata$Wsigzyp=cut(  mapdata$Wsigzyp, breaks=c(-1, 0.2 ,0, 0.2,1), 
                                    labels=  c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Wsigzyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Wsigzyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Wsigzyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=       c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
                
              }
              
              if (input$seasonmq=="Year"){
                
                mapdata$Ysigzyp=cut(  mapdata$Ysigzyp, breaks=c(-1, 0.2 ,0, 0.2,1), 
                                    labels=  c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1"))
                
                COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Ysigzyp)
                
                leafletProxy("datamap",session )%>%
                  clearPopups() %>% 
                  clearControls()%>%
                  clearMarkers() %>%
                  addTiles() %>%
                  addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Ysigzyp), 
                                   
                                   
                                   
                                   popup = ~paste(
                                     paste('<b>', 'Value', '</b>',   Ysigzyp), 
                                     paste('<b>',  'Station', '</b>', station),
                                     paste('<b>',  'River', '</b>', river),
                                     
                                     
                                     sep = '<br/>'),
                                   popupOptions = popupOptions(closeButton = FALSE)
                  )  %>%    
                  
                  addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                  
                  
                  addLegend(position="topright", pal=COL,values=       c("-1 - -0.2" , "-0.2-0","0 - 0.2","0.2-1") )%>%
                  
                  addLayersControl(
                    baseGroups = c("Open Street Map", "Terrain Background"),
                    position = "topright",
                    options = layersControlOptions(collapsed = F)
                  )
                
                
                
              }
              
              
    
            }
          })
          
            
            observeEvent({input$go_2}, {    
              
              if(input$trendtypeperiod=="Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
              
              
              if(input$periodway=="Length of Maximum Period under Value"){
                
                if(input$quantiles=="70"){
                  
                  
                  mapdata$Q70_tmax_zyp=cut(  as.numeric(mapdata$Q70_tmax_zyp), breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q70_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL, values=      c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="75"){
                  
                  
                  mapdata$Q75_tmax_zyp=cut(  mapdata$Q75_tmax_zyp, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q75_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="80"){
                  
                  
                  mapdata$Q80_tmax_zyp=cut(  mapdata$Q80_tmax_zyp, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q80_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="85"){
                  
                  
                  mapdata$Q85_tmax_zyp=cut(  mapdata$Q85_tmax_zyp, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',  Q85_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="90"){
                  
                  
                  mapdata$Q90_tmax_zyp=cut(  mapdata$Q90_tmax_zyp, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q90_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="95"){
                  
                  
                  mapdata$Q95_tmax_zyp=cut(  mapdata$Q95_tmax_zyp, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q95_tmax_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q95_tmax_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q95_tmax_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                
                
              }
              
              if(input$periodway=="Sum of Days under Value"){
                
                if(input$quantiles=="70"){
                  
                  
                  mapdata$Q70_ld_zyp=cut(  mapdata$Q70_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q70_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="75"){
                  
                  
                  mapdata$Q75_ld_zyp=cut(  mapdata$Q75_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q75_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="80"){
                  
                  
                  mapdata$Q80_ld_zyp=cut(  mapdata$Q80_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q80_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="85"){
                  
                  
                  mapdata$Q85_ld_zyp=cut(  mapdata$Q85_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q85_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="90"){
                  
                  
                  mapdata$Q90_ld_zyp=cut(  mapdata$Q90_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',  Q90_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                if(input$quantiles=="95"){
                  
                  
                  mapdata$Q95_ld_zyp=cut(  mapdata$Q95_ld_zyp, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                  
                  COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q95_ld_zyp)
                  
                  leafletProxy("datamap",session )%>%
                    clearPopups() %>% 
                    clearControls()%>%
                    clearMarkers() %>%
                    addTiles() %>%
                    addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q95_ld_zyp), 
                                     
                                     
                                     
                                     popup = ~paste(
                                       paste('<b>', 'Value', '</b>',   Q95_ld_zyp), 
                                       paste('<b>',  'Station', '</b>', station),
                                       paste('<b>',  'River', '</b>', river),
                                       
                                       
                                       sep = '<br/>'),
                                     popupOptions = popupOptions(closeButton = FALSE)
                    )  %>%    
                    
                    addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                    addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                    
                    
                    addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                    
                    addLayersControl(
                      baseGroups = c("Open Street Map", "Terrain Background"),
                      position = "topright",
                      options = layersControlOptions(collapsed = F)
                    )
                  
                  
                  
                  
                  
                }
                
                
                
                
                
              }
              
            }
              
              
              if(input$trendtypeperiod=="Linear Model: Least Squares Approach"){
                
                if(input$periodway=="Length of Maximum Period under Value"){
                  
                  if(input$quantiles=="70"){
                    
                    
                    mapdata$Q70_tmax_lm=cut(  mapdata$Q70_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q70_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="75"){
                    
                    
                    mapdata$Q75_tmax_lm=cut(  mapdata$Q75_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q75_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="80"){
                    
                    
                    mapdata$Q80_tmax_lm=cut(  mapdata$Q80_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q80_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="85"){
                    
                    
                    mapdata$Q85_tmax_lm=cut(  mapdata$Q85_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q85_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="90"){
                    
                    
                    mapdata$Q90_tmax_lm=cut(  mapdata$Q90_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q90_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="95"){
                    
                    
                    mapdata$Q95_tmax_lm=cut(  mapdata$Q95_tmax_lm, breaks=c(-7,-2,0,2,7), 
                                             labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q95_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q95_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q95_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  
                  
                }
                
                if(input$periodway=="Sum of Days under Value"){
                  
                  if(input$quantiles=="70"){
                    
                    
                    mapdata$Q70_ld_lm=cut(  mapdata$Q70_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q70_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="75"){
                    
                    
                    mapdata$Q75_ld_lm=cut(  mapdata$Q75_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q75_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="80"){
                    
                    
                    mapdata$Q80_ld_lm=cut(  mapdata$Q80_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q80_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="85"){
                    
                    
                    mapdata$Q85_ld_lm=cut(  mapdata$Q85_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q85_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="90"){
                    
                    
                    mapdata$Q90_ld_lm=cut(  mapdata$Q90_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q90_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="95"){
                    
                    
                    mapdata$Q95_ld_lm=cut(  mapdata$Q95_ld_lm, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q95_ld_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q95_ld_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q95_ld_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  
                  
                  
                }
                
                
                
                
              }
              
              if(input$trendtypeperiod=="Significance of Zyp-Trend"){
                
                if(input$periodway=="Length of Maximum Period under Value"){
                  
                  if(input$quantiles=="70"){
                    
                    
                    mapdata$Q70sigtmax=cut(  mapdata$Q70sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70sigtmax)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70sigtmax), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q70sigtmax), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="75"){
                    
                    
                    mapdata$Q75sigtmax=cut(  mapdata$Q75sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75sigtmax)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75sigtmax), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q75sigtmax), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="80"){
                    
                    
                    mapdata$Q80sigtmax=cut(  mapdata$Q80sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80sigtmax)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80sigtmax), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  
                                               Q80sigtmax), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="85"){
                    
                    
                    mapdata$Q85sigtmax=cut(  mapdata$Q85sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85sigtmax)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85sigtmax), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q85sigtmax), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="90"){
                    
                    
                    mapdata$Q90sigtmax=cut(  mapdata$Q90sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90_tmax_lm)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90_tmax_lm), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q90_tmax_lm), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="95"){
                    
                    
                    mapdata$Q95sigtmax=cut(  mapdata$Q95sigtmax, breaks=c(-7,-2,0,2,7), 
                                            labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q95sigtmax)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q95sigtmax), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q95sigtmax), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  
                  
                }
                
                if(input$periodway=="Sum of Days under Value"){
                  
                  if(input$quantiles=="70"){
                    
                    
                    mapdata$Q70sigld=cut(  mapdata$Q70sigld, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q70sigld)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q70sigld), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q70sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="75"){
                    
                    
                    mapdata$Q75sigld=cut(  mapdata$Q75sigld, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q75sigld)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q75sigld), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q75sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="80"){
                    
                    
                    mapdata$Q80sigld=cut(  mapdata$Q80sigld, breaks=c(-7,-2,0,2,7), 
                                           labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q80sigld)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q80sigld), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',   Q80sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="85"){
                    
                    
                    mapdata$Q85sigld=cut(  mapdata$Q85sigld, breaks=c(-7,-2,0,2,7), 
                                          labels= c("<-2","<0",  ">0",  ">2"))
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q85sigld)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q85sigld), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q85sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values= c("<-5","<-4", "<-3", "<-2", "<-1","<0", ">0", ">1", ">2", ">3", ">4", ">5"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="90"){
                    
                    
                    mapdata$Q90sigld=cut(  mapdata$Q90sigld, breaks=c(-7,-2,0,7), 
                                           labels=    )
                    
                    COL <- colorFactor(palette = c("red", "orange", "blue", "lightgreen", "darkgreen"), domain=  mapdata$Q90sigld)
                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~COL(Q90sigld), 
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q90sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright", pal=COL,values=  c("<-2","<0",  ">0",  ">2"))%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  if(input$quantiles=="95"){
                    
                    pal <- colorNumeric(
                      palette = "YlGnBu",
                      domain =mapdata$"Q95sigld"
                    )
               
                    

                    
                    leafletProxy("datamap",session )%>%
                      clearPopups() %>% 
                      clearControls()%>%
                      clearMarkers() %>%
                      addTiles() %>%
                      addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Q95sigld),
                                       
                                       
                                       
                                       popup = ~paste(
                                         paste('<b>', 'Value', '</b>',  Q95sigld), 
                                         paste('<b>',  'Station', '</b>', station),
                                         paste('<b>',  'River', '</b>', river),
                                         
                                         
                                         sep = '<br/>'),
                                       popupOptions = popupOptions(closeButton = FALSE)
                      )  %>%    
                      
                      addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                      addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                      
                      
                      addLegend(position="topright",  pal = pal, values = ~Q95sigld
                                )%>%
                      
                      addLayersControl(
                        baseGroups = c("Open Street Map", "Terrain Background"),
                        position = "topright",
                        options = layersControlOptions(collapsed = F)
                      )
                    
                    
                    
                    
                    
                  }
                  
                  
                  
                }
                
                
                
                
              }
            })
            
            
            
            
            
            
            
         
          
          
          
        }) 
          
        })
        
      })
      
      
      
      
    

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# Reset -------------------------------------------------------------------

  observeEvent({input$reset2},{
    session$reload()
  })  
  
observeEvent({input$reset},{
  session$reload()
})

  
  
  # third Page --------------------------------------------------------------
  
  
  
  # Map, distribution of stations -------------------------------------------
  
  
  
  
 
    
    

    
      map = createLeafletMap(session, "stationmap")
      
      session$onFlushed(once = T, function() {
        
    
    
    
    output$stationmap <- renderLeaflet({
      leaflet(data) %>%
        clearPopups() %>% 
        clearMarkers() %>%
        addTiles() %>%
        
        addCircleMarkers(lat = ~latitude, lng = ~longitude, 
                         
                         
                         
                         popup = ~paste(
                           paste('<b>', 'River', '</b>', river), 
                           paste('<b>',  'Station', '</b>', station),
                           paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                           
                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        ) 
      
      
      
  
    })
    
    
  })  
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Settings Select ---------------------------------------------------------
  
  observeEvent({input$dataselect}, {
    if(input$dataselect=="Representative Stations only"){
      
      l=  length(data$station)
      
      iden=rep(F,l)
      for ( i in 1:l){
        iden[i]=is.element(data$station[i], repres)
        
      }
      if(any(iden)==T){
        
        filtdata=data[which(iden==T),] 
        
        
        
        startyear=input$range[1]
        
        endyear=input$range[2]
        
        
        l=nrow(filtdata) #all stations, included in measurements
        
        
        
        
        
        stations_s=rep(F,l)
        stations_e=rep(F,l)
        for ( i in 1:l){
          stations_s[i]=filtdata$startyear[i]<=startyear  #measurements at least as long as given timeseries
          stations_e[i]=filtdata$endyear[i]>=endyear
        }
        
        start=which(stations_s == TRUE)
        end=which(stations_e == TRUE)
        l=length(start)
        vec=rep(F,l)
        for ( i in 1:l){
          if (identical(which(end==start[i]), integer(0))){
            vec[i]=F
          }else{ vec[i]=T}
          
        }
        timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
        l=length(timeseries)
        filtdata=filtdata[timeseries,]
        
        
        
        
        
        
        
        
        
        
        
      }else {renderText("No Stations available")}
      
      
      
      
      
      
      
      
      
      
      
      
      
      leafletProxy("stationmap",session, data=filtdata )%>%
        clearPopups() %>% 
        clearMarkers() %>%
        addTiles() %>%
        addCircleMarkers(data=filtdata, lat = ~latitude, lng = ~longitude, 
                         
                         
                         
                         popup = ~paste(
                           paste('<b>', 'River', '</b>', river), 
                           paste('<b>',  'Station', '</b>', station),
                           paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                           
                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )
    }else if(input$dataselect=="All GRDC-Stations in Germany"){
      
      
      filtdata=data
      
      
      
      
      startyear=input$range[1]
      
      endyear=input$range[2]
      
      
      l=nrow(filtdata) #all stations, included in measurements
      
      
      
      
      
      stations_s=rep(F,l)
      stations_e=rep(F,l)
      for ( i in 1:l){
        stations_s[i]=filtdata$startyear[i]<=startyear  #measurements at least as long as given timeseries
        stations_e[i]=filtdata$endyear[i]>=endyear
      }
      
      start=which(stations_s == TRUE)
      end=which(stations_e == TRUE)
      l=length(start)
      vec=rep(F,l)
      for ( i in 1:l){
        if (identical(which(end==start[i]), integer(0))){
          vec[i]=F
        }else{ vec[i]=T}
        
      }
      timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
      l=length(timeseries)
      filtdata=filtdata[timeseries,]
      
      
      
      
      
      
      
      
      
      leafletProxy("stationmap",session, data=filtdata )%>%
        clearPopups() %>% 
        clearMarkers() %>%
        addTiles() %>%
        addCircleMarkers(data=filtdata, lat = ~latitude, lng = ~longitude, 
                         
                         
                         
                         popup = ~paste(
                           paste('<b>', 'River', '</b>', river), 
                           paste('<b>',  'Station', '</b>', station),
                           paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                           
                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )
      
      
      
      
      
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    observeEvent({input$range},{
      #updateSliderInput(session, "yearq", label = "Select Year:",
      #                 min = sta_yea_cla, max = end_yea_cla)
      updateSliderInput(session, "range", "Select Timerange:"
      )
      
      STA=input$range[1]
      END=input$range[2]
      
      
      l=nrow(filtdata) #all stations, included in measurements
      
      
      
      
      
      stations_s=rep(F,l)
      stations_e=rep(F,l)
      for ( i in 1:l){
        stations_s[i]=filtdata$startyear[i]<= STA  #measurements at least as long as given timeseries
        stations_e[i]=filtdata$endyear[i]>=  END
      }
      
      start=which(stations_s == TRUE)
      end=which(stations_e == TRUE)
      l=length(start)
      vec=rep(F,l)
      for ( i in 1:l){
        if (identical(which(end==start[i]), integer(0))){
          vec[i]=F
        }else{ vec[i]=T}
        
      }
      timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included
      l=length(timeseries)
      filtdata=filtdata[timeseries,]
      
      
      
      
      
      
      
      leafletProxy("stationmap",session )%>%
        clearPopups() %>% 
        clearMarkers() %>%
        addTiles() %>%
        addCircleMarkers(data=filtdata, lat = ~latitude, lng = ~longitude, 
                         
                         
                         
                         popup = ~paste(
                           paste('<b>', 'River', '</b>', river), 
                           paste('<b>',  'Station', '</b>', station),
                           paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                           
                           sep = '<br/>'),
                         popupOptions = popupOptions(closeButton = FALSE)
        )  %>%    
        
        addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
        
        
        
        
        addLayersControl(
          baseGroups = c("Open Street Map", "Terrain Background"),
          position = "topright",
          options = layersControlOptions(collapsed = F)
        )
      
      
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  

# Distribution Graph ------------------------------------------------------

  
      
     # selectInput("ddgraph", "Data Distribution Graph", choices=c("Length: Timeseries of Discharge Data", "Area Distribution" )), 
      
      
     # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'",  radioButtons("densl", "Presentation", choices=c("Density Plot","Colour Map")))
      
      
      
  observeEvent({input$ddgraph}, {
    
    
    
    
    if (input$ddgraph=="Length: Timeseries of Discharge Data"){
  
   
          
          observe({
            
            
            if (input$densl=="Density Plot"){
              plot=length_distribution(data, "j")
              
              output$distplot=renderPlot({  plot})
              
            }
            if (input$densl=="Colour Map"){
              plot=length_distribution(data, "map")
              output$tmap=renderTmap({ plot })
              
            }
            
            
          })
      
    }
    
    if (input$ddgraph=="Compare Discharge Measurements"){
      observe({
        startyear2=input$yeatise[1]
        endyear2=input$yeatise[2]
        frame_1=input$frametise[1]
        frame_2=input$frametise[2]
        
        haha=tiseger(data, data2, startyear2, endyear2, frame_1, frame_2)
        output$tisepl= renderPlot({haha})
      })

      

      

                            
    }
    if (input$ddgraph=="Area Distribution"){
      

        
       area_plot=area_dist(data)
        output$areapl= renderPlot({area_plot})
      

    }

    
     
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      
      
      
      
}











shinyApp(ui=ui, server=server)
#springfunctions abchecken
#slope ausformulieren in caption 
#Achsenbeschriftung
#anders Runden, kleine Flüsse sehen nicht sinnvoll aus ........
# Input Dataset: Timerange and class of stations --------------------------

Periodmeta[["1820-2019"]]$Q95sigld
NMxQlist7[[1]]
Periodmeta[[1]]
