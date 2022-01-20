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
  
  
  
  
  
  # Map ---------------------------------------------------------------------
  
  
  map = createLeafletMap(session, 'map')
  
  session$onFlushed(once = T, function() {
    
    output$map <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addMarkers(lat = ~latitude, lng = ~longitude, 
                   clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T), 
                   popup = ~paste(
                     paste('<b>', 'River', '</b>', river), 
                     paste('<b>',  'longitude', '</b>', longitude),
                     paste('<b>',  'Length of Measurement [years]:', '</b>', d_years ),
                     paste('<b>', 'Catchment Area [km^2]:', '</b>', catch_area),
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
  
  
  # Add Table ---------------------------------------------------------------
  
  
  output$table_input=DT::renderDataTable({
    DT::datatable(data, selection='single', rownames=FALSE,colnames= c("grdc_no"="grdc_no" , "River"=   "river"   ,"Station"=   "station", "Country"=    "country", 
                                                                       "Catchment"="catch_area", "Altitude"= "altitude" , "Startyear"= "startyear" , "Endyear"= "endyear" ,
                                                                       "Time span"=   "d_years"   , "Longitude"= "longitude"  , "Latitude"="latitude"   ), filter="top",
                  options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr")
                  
                  
    )
  })
  
  
  
  
  # reactive Values
  
  
  
  
  
  #Initial conditions: 'Select station on map.'
  t_plot <- function(){
    
    plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select as station ", line = -1, cex = 1.5)
    
  }
  
  
  
  
  
  
  
  output$disch_plot <- renderPlot({t_plot()})
  
  
  
  empty=   function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    return(plot)
    
  }
  
  trendplot= function(){
    
    plot=plot(1:10, 1:10, type = "n", axes = F, ylab = "", xlab = "")
    mtext("Please select a station ", line = -1, cex = 1.5)
    return(plot)
    
  
  }
  
  
  
  
  
  # Erwins Reactive Map -----------------------------------------------------
  
  
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
      
      if(input$qplot_variety == "Trendplot"){
        
     mintrend=Qmin_trend(data2, stat_name)
     return(mintrend)
      
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
 
    
    
    

      
    output$trendplot=renderPlot({Qmin_trend(data2, stat_name)})
    
    
    
  })
  observeEvent(input$cleardata, {
    output$disch_plot=renderPlot({empty()})
  })
  
  
  
  
  
  #Dummy which gets selected gauge
  gauge_sel <-  shiny::reactiveValues(clicked_gauge = "XXX")
  
  #Reaction to selection of station on map
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
        
        output$disch_plot <- renderPlot({seasonplot})
      })
      
      
      
      
      
    }
    
  })
  observeEvent(input$cleardata, {
    output$disch_plot=renderPlot({empty()})
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui=ui, server=server)



