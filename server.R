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
    
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    
  }
  
  
  
  
  
  
  
  output$disch_plot <- renderPlot({t_plot()})
  
  
  
  empty=   function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    return(plot)
    
  }
  
  trendpl= function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station. ", line = -1, cex = 1.5)
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
 
    
    trendpl=function(){
    if (input$trendtype=="Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
      mintr=Qmin_trend(data2, stat_name, mod=2)
      return(mintr)
      
    }
      if (input$trendtype=="Linear Model: Least Squares Approach"){
        mintr=Qmin_trend(data2, stat_name, mod=3)
        return(mintr)
        
      }
      if (input$trendtype=="Yuepilon-Method and Linear Approach"){
        mintr=Qmin_trend(data2, stat_name, mod=1)
        return(mintr)
        
      }}
      
    output$trendplot=renderPlot({trendpl()})
    
    

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
    
    
    trendpl=function(){
      if (input$trendtype=="Yuepilon-Method: PreWhitening and homogenization of autocorrelation"){
        mintr=Qmin_trend(data2, stat_name, mod=2)
        return(mintr)
        
      }
      if (input$trendtype=="Linear Model: Least Squares Approach"){
        mintr=Qmin_trend(data2, stat_name, mod=3)
        return(mintr)
        
      }
      if (input$trendtype=="Yuepilon-Method and Linear Approach"){
        mintr=Qmin_trend(data2, stat_name, mod=1)
        return(mintr)
        
      }}
    
    output$trendplot=renderPlot({trendpl()})

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

  map = createLeafletMap(session, 'areamap')
  
  
  
  
  filtdata=data
  
  
  
  
  session$onFlushed(once = T, function() {
    
    

    
    
    output$areamap <- renderLeaflet({
      leaflet(filtdata) %>%
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
  
  
  


# Inputs ------------------------------------------------------------------
#1.range
  

  
  
  
  observeEvent({input$dataset}, {
    if(input$dataset=="Representative Stations only"){
    
    l=  length(data$station)
    
    iden=rep(F,l)
    for ( i in 1:l){
      iden[i]=is.element(data$station[i], repres)
      
    }
    if(any(iden)==T){
      
      filtdata=data[which(iden==T),] 
    }else {renderText("No Stations available")}
    
    
    
   
    
    }
    
    
    
    
    
    
    
    leafletProxy("areamap",session, data=filtdata )%>%
      clearPopups() %>% 
      clearMarkers() %>%
      addTiles() %>%
      addCircleMarkers(data=data, lat = ~latitude, lng = ~longitude, 
                       
                       
                       
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







    observeEvent({input$range},{
      
      
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
      
      leafletProxy("areamap",session, data=datanew )%>%
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
    
    

     
      
 
      
      
      
      

  

  
  
  
  
}
  
  
  
  
  
  
   
      
      
      
     


    
   
  
 
  
  

  
    


shinyApp(ui=ui, server=server)



