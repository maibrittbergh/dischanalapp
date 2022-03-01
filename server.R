library(dplyr)
install.packages(("tigris"))
library(tidyverse)
library(tigris)
library(leaflet)
install.packages("Lchiffon/leafletCN")
install.packages("viridis")
library(viridis)


server= function(input, output, session){
  

  
  # Introduction ------------------------------------------------------------
  
  
  
 # query_modal <- modalDialog(
  #  title = "Analyze Discharge Data with Dischanalyst",
   # "This App uses the GRDC-Dataset and the dischanalyst R-Package to analyze Discharge Measurements in Germany.
   #   If you have any Questions concering 
  #    the app's operation, please check the User's Guide. ",
    
  #  easyClose = F,
   # footer = tagList(
  #    actionButton("start_window", "Explore")
  #  )
  #)
  
  # Show the model on start up ...
#  showModal(query_modal)
  
 # observeEvent(input$start_window, {
  #  removeModal()
  #})
  
  
  
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
    
    mapd=data
    
    

    if (input$trendtype2=="MQ - Mean Discharge Trend"){
      mapd=MQlist
      
    }
    if (input$trendtype2=="Trend Minimum Values"){
      mapd=mintrendlist
      
    }
    if (input$trendtype2=="NMxQ"){
      
      
      
   
      if (input$xval=="14"){
        mapd=NMxQlist14
        
      }
      if (input$xval=="30"){
        mapd=NMxQlist30
        
      }
      if (input$xval=="60"){
        mapd=NMxQlist60
        
      }else{
      mapd=NMxQlist7}
      
      
    }
    if ( input$trendtype2=="Low Flow Period"){
      mapd=Periodmeta
    }
  
      
      
      observeEvent({input$timerange2}, {
        
        
        
        mapdata=mapd[[input$timerange2]]  
        
        
        
        
        
        
        
        
        
        leafletProxy("datamap",session, data=mapdata )%>%
          clearPopups() %>% 
          clearMarkers() %>%
          clearControls()%>%
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
        
              
              mapdata=mapdata[which(iden==T),] 
              
              
              
              
              
            
              

            
            
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
     

          
        
        })
      
          #season, color collected
          
          
          tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !;
    left:50;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
          
          
          tag.map.title2 <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !;
    left: 50;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
       #Meantrend   
    
       observeEvent(input$go,{
         
         
         if(input$dataset=="Representative Stations only"){
           
           l=  length(mapdata$station)
           
           iden=rep(F,l)
           for ( i in 1:l){
             iden[i]=is.element(mapdata$station[i], repres)
             
           }
           
           
           mapdata=mapdata[which(iden==T),] }else{
             
             mapdata=mapdata
             
           }
           
           
         
         
         
         
         
         
         
         if(input$trendtypemq== "Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
           
           
           
           
           if (input$seasonmq=="Spring"){
             
             ######
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Spring - Zyp Trend of Mean Values")
             ) 
             
             Spzyp= as.numeric(mapdata$Spslopezyp)
             
             pal=colorNumeric("RdYlBu", domain=  Spzyp)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spzyp),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Spzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             
             ######
             
             
             
             
             
             
             
             
             
             
             
             
             
           }
           if (input$seasonmq=="Summer"){
             
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Summer - Zyp Trend of Mean Values ")
             ) 
             
             Szyp= as.numeric(mapdata$Sslopezyp)
             
             pal=colorNumeric("RdYlBu", domain=  Szyp)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Szyp),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Szyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
           }
           if (input$seasonmq=="Autumn"){
             
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Autumn - Zyp Trend of Mean Values")
             ) 
             
             Azyp= as.numeric(mapdata$Aslopezyp)
             
             pal=colorNumeric("RdYlBu", domain=  Azyp)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Azyp),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Azyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Winter"){
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Winter - Zyp Trend of Mean Values")
             ) 
             
             Wzyp= as.numeric(mapdata$Wslopezyp)
             
             pal=colorNumeric("RdYlBu", domain=  Wzyp)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wzyp),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Wzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
             
           }
           if (input$seasonmq=="Year"){
             
             #####
             
             title <- tags$div(
               tag.map.title2, HTML("   Annual Zyp Trend of Mean Values")
             ) 
             
             Yzyp= as.numeric(mapdata$Yslopezyp)
             
             pal=colorNumeric("RdYlBu", domain=  Yzyp)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Yzyp),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Yzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             #####
             
             
           }
           
           
           
         }
         
         
         
         if(input$trendtypemq== "Linear Model: Least Squares Approach"){
           
           if (input$seasonmq=="Spring"){
             
             
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Spring - Linear Model Trend of Mean Values")
             ) 
             
             Splm= as.numeric(mapdata$Spslopelm)
             
             pal=colorNumeric("RdYlBu", domain=  Splm)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Splm),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Splm, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             ####
             
             
             
           }
           if (input$seasonmq=="Summer"){
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Summer - Linear Model Trend of Mean Values ")
             ) 
             
             Slm= as.numeric(mapdata$Sslopelm)
             
             pal=colorNumeric("RdYlBu", domain=  Slm)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Slm),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Slm, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Autumn"){
             
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Autumn - Linear Model Trend of Mean Values")
             ) 
             
             Alm= as.numeric(mapdata$Aslopelm)
             
             pal=colorNumeric("RdYlBu", domain=  Alm)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Alm),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Alm, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Winter"){
             
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Winter - Linear Model Trend of Mean Values")
             ) 
             
             Wlm= as.numeric(mapdata$Wslopelm)
             
             pal=colorNumeric("RdYlBu", domain=  Wlm)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wlm),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Wlm, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Year"){
             
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Annual Linear Model Trend of Mean Values ")
             ) 
             
             Ylm= as.numeric(mapdata$Yslopelm)
             
             pal=colorNumeric("RdYlBu", domain=  Ylm)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ylm),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Ylm, title="Slope", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             #####
             
             
             
           }
         }
         
         #mapdata$Spsigzyp)
         if(input$trendtypemq== "Significance of Zyp-Trend"){
           
           if (input$seasonmq=="Spring"){
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Spring - Significance Zyp Trend ")
             ) 
             
             Spsig_= as.numeric(mapdata$Spsigzyp)
             
             pal=colorNumeric("viridis", reverse = T, domain=  Spsig_)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spsig_),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Spsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Summer"){
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Summer - Significance Zyp Trend ")
             ) 
             
             Ssig_= as.numeric(mapdata$Ssigzyp)
             
             pal=colorNumeric("viridis", reverse = T,domain=  Ssig_)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ssig_),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Ssig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
           }
           if (input$seasonmq=="Autumn"){
             
             #####
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Autumn - Significance Zyp Trend ")
             ) 
             
             Asig_= as.numeric(mapdata$Asigzyp)
             
             pal=colorNumeric("viridis", reverse = T,domain=  Asig_)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Asig_),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Asig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             
             #####
             
             
             
             
           }
           if (input$seasonmq=="Winter"){
             
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Season: Winter - Significance Zyp Trend ")
             ) 
             
             Wsig_= as.numeric(mapdata$Wsigzyp)
             
             pal=colorNumeric("viridis", reverse = T,domain=  Wsig_)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wsig_),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Wsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             
             
             
             #####
             
             
             
             
             
           }
           
           if (input$seasonmq=="Year"){
             
             #####
             
             
             
             title <- tags$div(
               tag.map.title2, HTML("Annual Trend Significance Zyp  ")
             ) 
             
             Ysig_= as.numeric(mapdata$Ysigzyp)
             
             pal=colorNumeric("viridis", reverse = T,domain=  Ysig_)
             
             leafletProxy("datamap",session )%>%
               clearPopups() %>% 
               clearControls()%>%
               clearMarkers() %>%
               addTiles() %>%
               
               addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ysig_),
                                
                                
                                
                                popup = ~paste(
                                  
                                  paste('<b>',  'Station', '</b>', station),
                                  paste('<b>',  'River', '</b>', river),
                                  
                                  
                                  sep = '<br/>'),
                                popupOptions = popupOptions(closeButton = FALSE)
               )  %>%    
               
               addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
               addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
               
               
               
               addLegend(pal=pal, position="topleft", values=  Ysig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
               addControl(title, position="topright", className="map-title")%>%
               
               addLayersControl(
                 
                 baseGroups = c("Open Street Map", "Terrain Background"),
                 position = "topright",
                 
                 options = layersControlOptions(collapsed = F)
               )
             #####
             
             
             
             
             
             
             
             
             
           }
           
           
           
         }
         
         
       })
       #Mintrend
       
       
         
         observeEvent(input$go_mintrend,{
           
           if(input$dataset=="Representative Stations only"){
             
             l=  length(mapdata$station)
             
             iden=rep(F,l)
             for ( i in 1:l){
               iden[i]=is.element(mapdata$station[i], repres)
               
             }
             
             
             mapdata=mapdata[which(iden==T),] }else{
               
               mapdata=mapdata
               
             }
           
           
       
           
           
           
           if(input$trendtypemq3== "Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
             
             
             
             
             if (input$seasonmq3=="Spring"){
               
               ######
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Zyp Trend of Minumum Values")
               ) 
               
               Spzyp= as.numeric(mapdata$Spslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Spzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Spzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               
               ######
               
               
               
               
               
               
               
               
               
               
               
               
               
             }
             if (input$seasonmq3=="Summer"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Zyp Trend of Minumum Values ")
               ) 
               
               Szyp= as.numeric(mapdata$Sslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Szyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Szyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Szyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             }
             if (input$seasonmq3=="Autumn"){
               
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Zyp Trend of Minumum Values")
               ) 
               
               Azyp= as.numeric(mapdata$Aslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Azyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Azyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Azyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Winter"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Zyp Trend of Minumum Values")
               ) 
               
               Wzyp= as.numeric(mapdata$Wslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Wzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
             }
             if (input$seasonmq3=="Year"){
               
               #####
               
               title <- tags$div(
                 tag.map.title2, HTML("   Annual Zyp Trend of Minumum Values")
               ) 
               
               Yzyp= as.numeric(mapdata$Yslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Yzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Yzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Yzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               #####
               
               
             }
             
             
             
           }
           
           
           
           if(input$trendtypemq3== "Linear Model: Least Squares Approach"){
             
             if (input$seasonmq3=="Spring"){
               
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Linear Model Trend of Minumum Values")
               ) 
               
               Splm= as.numeric(mapdata$Spslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Splm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Splm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Splm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               ####
               
               
               
             }
             if (input$seasonmq3=="Summer"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Linear Model Trend of Minumum Values ")
               ) 
               
               Slm= as.numeric(mapdata$Sslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Slm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Slm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Slm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Autumn"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Linear Model Trend of Minumum Values")
               ) 
               
               Alm= as.numeric(mapdata$Aslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Alm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Alm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Alm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Winter"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Linear Model Trend of Minumum Values")
               ) 
               
               Wlm= as.numeric(mapdata$Wslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Wlm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wlm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wlm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Year"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Annual Linear Model Trend of Minumum Values ")
               ) 
               
               Ylm= as.numeric(mapdata$Yslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Ylm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ylm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ylm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               #####
               
               
               
             }
           }
           
           #mapdata$Spsigzyp)
           if(input$trendtypemq3== "Significance of Zyp-Trend"){
             
             if (input$seasonmq3=="Spring"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Significance Zyp Trend ")
               ) 
               
               Spsig_= as.numeric(mapdata$Spsigzyp)
               
               pal=colorNumeric("viridis", reverse = T, domain=  Spsig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spsig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Spsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Summer"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Significance Zyp Trend ")
               ) 
               
               Ssig_= as.numeric(mapdata$Ssigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Ssig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ssig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ssig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq3=="Autumn"){
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Significance Zyp Trend ")
               ) 
               
               Asig_= as.numeric(mapdata$Asigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Asig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Asig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Asig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
               
             }
             if (input$seasonmq3=="Winter"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Significance Zyp Trend ")
               ) 
               
               Wsig_= as.numeric(mapdata$Wsigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Wsig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wsig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
               
             }
             
             if (input$seasonmq3=="Year"){
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Annual Trend Significance Zyp  ")
               ) 
               
               Ysig_= as.numeric(mapdata$Ysigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Ysig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ysig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ysig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               #####
               
               
               
               
               
               
               
               
               
             }
             
             
             
           }
         
          
           })
       
         
          ###### NMxQ
        
 
 
         observeEvent(input$go_NMxQ,{
           
           if(input$dataset=="Representative Stations only"){
             
             l=  length(mapdata$station)
             
             iden=rep(F,l)
             for ( i in 1:l){
               iden[i]=is.element(mapdata$station[i], repres)
               
             }
             
             
             mapdata=mapdata[which(iden==T),] }else{
               
               mapdata=mapdata
               
             }
           

           
           
           if(input$trendtypemq2== "Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
             
             
             
             
             if (input$seasonmq2=="Spring"){
               
               ######
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Zyp Trend of MNxQ Values")
               ) 
               
               Spzyp= as.numeric(mapdata$Spslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Spzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Spzyp, title="Slope ", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               
               ######
               
               
               
               
               
               
               
               
               
               
               
               
               
             }
             if (input$seasonmq2=="Summer"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Zyp Trend of MNxQ Values ")
               ) 
               
               Szyp= as.numeric(mapdata$Sslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Szyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Szyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Szyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             }
             if (input$seasonmq2=="Autumn"){
               
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Zyp Trend of MNxQ Values")
               ) 
               
               Azyp= as.numeric(mapdata$Aslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Azyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Azyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Azyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Winter"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Zyp Trend of MNxQ Values")
               ) 
               
               Wzyp= as.numeric(mapdata$Wslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Wzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
             }
             if (input$seasonmq2=="Year"){
               
               #####
               
               title <- tags$div(
                 tag.map.title2, HTML("   Annual Zyp Trend of MNxQ Values")
               ) 
               
               Yzyp= as.numeric(mapdata$Yslopezyp)
               
               pal=colorNumeric("RdYlBu", domain=  Yzyp)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Yzyp),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Yzyp, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               #####
               
               
             }
             
             
             
           }
           
           
           
           if(input$trendtypemq2== "Linear Model: Least Squares Approach"){
             
             if (input$seasonmq2=="Spring"){
               
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Linear Model Trend of MNxQ Values")
               ) 
               
               Splm= as.numeric(mapdata$Spslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Splm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Splm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Splm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               ####
               
               
               
             }
             if (input$seasonmq2=="Summer"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Linear Model Trend of MNxQ Values ")
               ) 
               
               Slm= as.numeric(mapdata$Sslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Slm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Slm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Slm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Autumn"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Linear Model Trend of MNxQ Values")
               ) 
               
               Alm= as.numeric(mapdata$Aslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Alm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Alm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Alm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Winter"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Linear Model Trend of MNxQ Values")
               ) 
               
               Wlm= as.numeric(mapdata$Wslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Wlm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wlm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wlm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Year"){
               
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Annual Linear Model Trend of MNxQ Values ")
               ) 
               
               Ylm= as.numeric(mapdata$Yslopelm)
               
               pal=colorNumeric("RdYlBu", domain=  Ylm)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ylm),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ylm, title="Slope", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               #####
               
               
               
             }
           }
           
           #mapdata$Spsigzyp)
           if(input$trendtypemq2== "Significance of Zyp-Trend"){
             
             if (input$seasonmq2=="Spring"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Spring - Significance Zyp Trend ")
               ) 
               
               Spsig_= as.numeric(mapdata$Spsigzyp)
               
               pal=colorNumeric("viridis", reverse = T, domain=  Spsig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Spsig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Spsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Summer"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Summer - Significance Zyp Trend ")
               ) 
               
               Ssig_= as.numeric(mapdata$Ssigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Ssig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ssig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ssig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
             }
             if (input$seasonmq2=="Autumn"){
               
               #####
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Autumn - Significance Zyp Trend ")
               ) 
               
               Asig_= as.numeric(mapdata$Asigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Asig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Asig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Asig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               
               #####
               
               
               
               
             }
             if (input$seasonmq2=="Winter"){
               
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Season: Winter - Significance Zyp Trend ")
               ) 
               
               Wsig_= as.numeric(mapdata$Wsigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Wsig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Wsig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Wsig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               
               
               
               #####
               
               
               
               
               
             }
             
             if (input$seasonmq2=="Year"){
               
               #####
               
               
               
               title <- tags$div(
                 tag.map.title2, HTML("Annual Trend Significance Zyp  ")
               ) 
               
               Ysig_= as.numeric(mapdata$Ysigzyp)
               
               pal=colorNumeric("viridis", reverse = T,domain=  Ysig_)
               
               leafletProxy("datamap",session )%>%
                 clearPopups() %>% 
                 clearControls()%>%
                 clearMarkers() %>%
                 addTiles() %>%
                 
                 addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(Ysig_),
                                  
                                  
                                  
                                  popup = ~paste(
                                    
                                    paste('<b>',  'Station', '</b>', station),
                                    paste('<b>',  'River', '</b>', river),
                                    
                                    
                                    sep = '<br/>'),
                                  popupOptions = popupOptions(closeButton = FALSE)
                 )  %>%    
                 
                 addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                 addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                 
                 
                 
                 addLegend(pal=pal, position="topleft", values=  Ysig_, title="Kendall's P-Value", labFormat = labelFormat(digits = 6))%>%
                 addControl(title, position="topright", className="map-title")%>%
                 
                 addLayersControl(
                   
                   baseGroups = c("Open Street Map", "Terrain Background"),
                   position = "topright",
                   
                   options = layersControlOptions(collapsed = F)
                 )
               #####
               
               
               
               
               
               
               
               
               
             }
             
             
             
           }
         
           
            })
          
         
         #####Minimum Period 
         
          
    observeEvent(input$go_2,{    
           
      
      if(input$dataset=="Representative Stations only"){
        
        l=  length(mapdata$station)
        
        iden=rep(F,l)
        for ( i in 1:l){
          iden[i]=is.element(mapdata$station[i], repres)
          
        }
        
        
        mapdata=mapdata[which(iden==T),] }else{
          
          mapdata=mapdata
          
        }

           
           if(input$trendtypeperiod=="Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
             
             
             
             
             
             
             
             if(input$periodway=="Length of Maximum Period under Value"){
               
               if(input$quantiles=="70"){
                 
                 
                 
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q70) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ70= as.numeric(mapdata$Q70_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T,domain=    tmaxzypQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ70, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 
                 
                 #####
                 
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q75) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ75= as.numeric(mapdata$Q75_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ75, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
                 
                 
                 ######
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 
                 
                 
                 #####
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q80) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ80= as.numeric(mapdata$Q80_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ80, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   
                   
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 ####
                 
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 
                 
                 
                 #####
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q85) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ85= as.numeric(mapdata$Q85_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ85, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q90) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ90= as.numeric(mapdata$Q90_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ90, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ####
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 #####
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold (Q95) - Zyp Trend")
                 ) 
                 
                 tmaxzypQ95= as.numeric(mapdata$Q95_tmax_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxzypQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(tmaxzypQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=  tmaxzypQ95, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 ####
                 
                 
                 
                 
                 
                 
                 
               }
               
               
             }
             
             if(input$periodway=="Sum of Days under Value"){
               
               if(input$quantiles=="70"){
                 #####
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q70) - Zyp Trend")
                 ) 
                 
                 ldzypQ70= as.numeric(mapdata$Q70_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ70, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 ####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 
                 
                 #######
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q75) - Zyp Trend")
                 ) 
                 
                 ldzypQ75= as.numeric(mapdata$Q75_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ75, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #######
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 #####
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q80) - Zyp Trend")
                 ) 
                 
                 ldzypQ80= as.numeric(mapdata$Q80_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ80, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 
                 
                 
                 ######
                 
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q85) - Zyp Trend")
                 ) 
                 
                 ldzypQ85= as.numeric(mapdata$Q85_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ85, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 
                 ######
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q90) - Zyp Trend")
                 ) 
                 
                 ldzypQ90= as.numeric(mapdata$Q90_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ90, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 
                 
                 
                 #######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q95) - Zyp Trend")
                 ) 
                 
                 ldzypQ95= as.numeric(mapdata$Q95_ld_zyp)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=     ldzypQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(   ldzypQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldzypQ95, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ######
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               }
               
               
               
               
               
             }
             
           }
           
           
           if(input$trendtypeperiod=="Linear Model: Least Squares Approach"){
             
             if(input$periodway=="Length of Maximum Period under Value"){
               
               if(input$quantiles=="70"){
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q70) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ70= as.numeric(mapdata$Q70_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ70, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 ######
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 
                 
                 
                 ######
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q75) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ75= as.numeric(mapdata$Q75_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ75, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 #######
                 
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 
                 
                 #######
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q80) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ80= as.numeric(mapdata$Q80_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ80, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
                 #######
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q85) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ85= as.numeric(mapdata$Q85_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ85, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 ##########
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 
                 #########
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q90) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ90= as.numeric(mapdata$Q90_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ90, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
                 #########
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 
                 
                 #######
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Maximum Timespan under Threshold(Q95) - Linear Model Trend")
                 ) 
                 
                 tmaxlmQ95= as.numeric(mapdata$Q95_tmax_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    tmaxlmQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  tmaxlmQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    tmaxlmQ95, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 #######
                 
                 
                 
                 
                 
                 
               }
               
               
             }
             
             if(input$periodway=="Sum of Days under Value"){
               
               if(input$quantiles=="70"){
                 
                 
                 
                 #####
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q70) - Linear Model Trend")
                 ) 
                 
                 ldlmQ70= as.numeric(mapdata$Q70_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ70, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 
                 
                 #######
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q75) - Linear Model Trend")
                 ) 
                 
                 ldlmQ75= as.numeric(mapdata$Q75_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ75, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 ######
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 
                 #######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q80) - Linear Model Trend")
                 ) 
                 
                 ldlmQ80= as.numeric(mapdata$Q80_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ80, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #######
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q85) - Linear Model Trend")
                 ) 
                 
                 ldlmQ85= as.numeric(mapdata$Q85_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ85, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 #####
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q90) - Linear Model Trend")
                 ) 
                 
                 ldlmQ90= as.numeric(mapdata$Q90_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ90, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 ######
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Sum of Days under Threshold(Q95) - Linear Model Trend")
                 ) 
                 
                 ldlmQ95= as.numeric(mapdata$Q95_ld_lm)
                 
                 pal=colorNumeric("RdYlBu", reverse=T, domain=    ldlmQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( ldlmQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=    ldlmQ95, title="Slope")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ####
                 
                 
                 
                 
               }
               
               
               
             }
             
             
           }
           
           if(input$trendtypeperiod=="Significance of Zyp-Trend"){
             
             if(input$periodway=="Length of Maximum Period under Value"){
               
               if(input$quantiles=="70"){
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q70) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ70= as.numeric(mapdata$Q70sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ70, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 ######
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q75) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ75= as.numeric(mapdata$Q75sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ75, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q80) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ80= as.numeric(mapdata$Q80sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ80, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ######
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q85) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ85= as.numeric(mapdata$Q85sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ85, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ######
                 
                 
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q90) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ90= as.numeric(mapdata$Q90sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ90, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 ######
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 
                 ######
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Maximum Timespan under Threshold(Q95) - Zyp Trend")
                 ) 
                 
                 tmaxsigQ95= as.numeric(mapdata$Q95sigtmax)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    tmaxsigQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal( tmaxsigQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   tmaxsigQ95, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
                 
               }
               
               
             }
             
             if(input$periodway=="Sum of Days under Value"){
               
               if(input$quantiles=="70"){
                 
                 
                 #####
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q70) - Zyp Trend")
                 ) 
                 
                 ldsigQ70= as.numeric(mapdata$Q70sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ70)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ70),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ70,title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="75"){
                 
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q75) - Zyp Trend")
                 ) 
                 
                 ldsigQ75= as.numeric(mapdata$Q75sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ75)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ75),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ75, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="80"){
                 
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q80) - Zyp Trend")
                 ) 
                 
                 ldsigQ80= as.numeric(mapdata$Q80sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ80)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ80),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ80, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
               }
               if(input$quantiles=="85"){
                 
                 #####
                 
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q85) - Zyp Trend")
                 ) 
                 
                 ldsigQ85= as.numeric(mapdata$Q85sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ85)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ85),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ85,title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="90"){
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q90) - Zyp Trend")
                 ) 
                 
                 ldsigQ90= as.numeric(mapdata$Q90sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ90)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ90),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ90, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
                 
                 
               }
               if(input$quantiles=="95"){
                 
                 
                 
                 #####
                 
                 
                 title <- tags$div(
                   tag.map.title, HTML("Significance: Sum of Days under Threshold(Q95) - Zyp Trend")
                 ) 
                 
                 ldsigQ95= as.numeric(mapdata$Q95sigld)
                 
                 pal=colorNumeric("viridis", reverse = T, domain=    ldsigQ95)
                 
                 leafletProxy("datamap",session )%>%
                   clearPopups() %>% 
                   clearControls()%>%
                   clearMarkers() %>%
                   addTiles() %>%
                   addCircleMarkers(data=mapdata, lat = ~latitude, lng = ~longitude, color=~pal(  ldsigQ95),
                                    
                                    
                                    
                                    popup = ~paste(
                                      
                                      paste('<b>',  'Station', '</b>', station),
                                      paste('<b>',  'River', '</b>', river),
                                      
                                      
                                      sep = '<br/>'),
                                    popupOptions = popupOptions(closeButton = FALSE)
                   )  %>%    
                   
                   addProviderTiles(providers$OpenStreetMap.HOT,        group = "Open Street Map") %>%   
                   addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
                   
                   addLegend(pal=pal, position="topleft", values=   ldsigQ95, title="Kendall's P-Value")%>%
                   addControl(title, position="topright", className="map-title")%>%
                   
                   addLayersControl(
                     
                     baseGroups = c("Open Street Map", "Terrain Background"),
                     position = "topright",
                     
                     options = layersControlOptions(collapsed = F)
                   )
                 
                 
                 #####
                 
                 
                 
                 
               }
               
               
               
             }
             
             
             
             
           }
          })
          
            
            
    
            
            
            
        
         
   
   
          
        })

  })
  
  
  
      
  observeEvent(input$CD, {

    shinyjs::runjs("location.reload();")
   # session$reload()
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# Reset -------------------------------------------------------------------

  observeEvent({input$reset2},{
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
