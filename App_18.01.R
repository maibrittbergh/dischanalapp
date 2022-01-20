#actualapp
#data=metadata -startday, -endday
#data2=large List: contains every Date and Discharge Value for the station in metadata




#Pakete laden


#install.packages("leaflet")
#install.packages("tmaptools")

#remove.packages(c("tmaptools", "lwgeom"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('tmaptools', dependencies = TRUE)
#install.packages("shinythemes")
#install.packages('lwgeom', dependencies = TRUE)
library("shinythemes")
library(gridExtra)
library(tmaptools)
library(leaflet)
library(readxl)
library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
library(readr)

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.providers)
#library(meltimr)
library(zyp)
library(Kendall)
library(zoo)
library(readr)
library(viridisLite)
library(RColorBrewer)

library(sp)
library(rgdal)
#install.packages("DT")

library(DT) #make sure you load DT after Shiny

library(dischanalyst)





# Daten vorbereiten -------------------------------------------------------


#st_grdc=st_as_sf(metadata_repg, coords=c("longitude","latitude"), crs=4326 )

#data=st_grdc

data=metadata_repg[, -c(7,8)]

data2= grdc_list(metadata_repg, path)
#data3=GRDC_list(metadata_germany, path)







ui = navbarPage(title="Low Flow Analysis in Germany", theme = shinytheme("paper"),
                
                
                
                tabPanel(title="Discharge Map",
                         
                         fluidRow(
                           column(7, 
                                  conditionalPanel(condition="input.plot_tabs!='User guide'", 
                                                   tabsetPanel(id="ui_tab", 
                                                               
                                                               tabPanel("Map", 
                                                                        column(12, h4("Click a site"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                                    size=3, color="#0080b7"))), 
                                                               tabPanel("Table", 
                                                                        column(12, h4("Click a site"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))
                                                               ))
                                  ),
                                  conditionalPanel(condition="input.plot_tabs=='User guide'", column(12)
                                  )
                           ), #Abschließen der linken Spalte mit Tabelle und Map
                           
                           column(5, tabsetPanel(id="plot_tabs", 
                                                 tabPanel("Descriptive Statistics", 
                                                          fluidRow(column(10, 
                                                                          # uiOutput("date_slider") Vielleicht statt Jahr?
                                                                          
                                                                          
                                                                          
                                                                          radioButtons("ts_plot_type", "Plot type:", choices=c("Discharge Measurements", "Trend Analysis"), 
                                                                                       inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty 
                                                                          
                                                                          conditionalPanel(condition="input.ts_plot_type=='Discharge Measurements'", 
                                                                                           selectInput("qplot_variety", label="Display Options for Discharge Measurements:",
                                                                                                       choices=c("Discharge Plot",   "annual Discharge Plot", "annual Discharge Boxplot", "Discharge Boxplot",   "Seasonplot")) 
                                                                          ),
                                                                          conditionalPanel(condition="input.qplot_variety=='annual Discharge Boxplot'",  sliderInput("year", "Select Year:", 2000, min=1975, max=2015)),
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          conditionalPanel(condition="input.qplot_variety=='annual Discharge Plot'",  sliderInput("year2", "Select Year:", 2000, min=1975, max=2015) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season1", "Select Begin of the Season:",5,min=01, max=12)),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season2", "Select End of the Season:",5,min=01, max=12, ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("ssy", "Select Startyear:",2000, min=1999, max=2005 ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("sey", "Select Endyear:",2001, min=1999, max=2005 ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",      actionButton("printplot", label="Print Plot")  ),
                                                                          
                                                                          #inputs
                                                                          conditionalPanel(condition="input.ts_plot_type=='annual Discharge Boxplot'"
                                                                                           
                                                                                           
                                                                          ),
                                                                          conditionalPanel(condition="input.ts_plot_type=='Discharge Boxplot'"
                                                                                           
                                                                          ), 
                                                                          conditionalPanel(condition="input.ts_plot_type=='annual Discharge Plot'"
                                                                                           
                                                                          ), 
                                                                          conditionalPanel(condition="input.ts_plot_type=='Discharge Plot'"
                                                                                           
                                                                          ), 
                                                                          
                                                                          conditionalPanel(condition="input.ts_plot_type=='Seasonplot'"
                                                                                           
                                                                          ), 
                                                                          
                                                                          conditionalPanel(condition="input.cleardata"
                                                                                           
                                                                                           
                                                                          ), 
                                                                          
                                                                          plotOutput("disch_plot", width = "100%"), 
                                                                          
                                                                          actionButton("cleardata", label="Clear Data") 
                                                                          
                                                                    
                                                                          
                                                          )) ),
                                                 
                                                 
                                                 tabPanel("Threshold-based"
                                                          
                                                          
                                                 ), 
                                                 
                                                 
                                                 
                                                 
                                                 tabPanel("User guide",
                                                          fluidRow(
                                                            column(8,
                                                                   # includeMarkdown('./user_guide/user_guide.rmd') #including MArkdown for Users Guide 
                                                            )
                                                          )
                                                 )
                                                 
                           )))), 
                
                
                
                
                
                
                
                
                
                tabPanel(title="Trend of Minimum Discharge", leafletOutput("tmap_", height=1000)),
                
                
                
                
                
                tabPanel(title="Descriptive Statistics", 
                         fluidPage(
                           sidebarPanel(
                             
                             selectInput("rivername", "Name of the river",  data$river),
                             selectInput("stationname", "Name of the station",  
                                         data$station),      #Frage: warum geht das nicht: data$station[which(data$river== input$rivername)]   
                             numericInput("year", "Year", 2000, min=1975, max=2018) 
                             
                           ),
                           mainPanel({
                             textOutput("selected_rivername")
                             plotOutput("low_flow", height=1000)
                           }))), 
                
                
                tabPanel(title="Discharge Analysis", 
                         fluidPage(
                           sidebarPanel(
                             
                             selectInput("rivername", "Name of the river",  data$river),
                             selectInput("stationname", "Name of the station",  
                                         data$station),      #Frage: warum geht das nicht: data$station[which(data$river== input$rivername)]   
                             numericInput("year", "Year", 2000, min=1975, max=2018)  ,
                             
                             sliderInput("U", "Value", min=30, max=1000,
                                         500, 1)
                             
                             
                             
                           ),
                           
                           
                           mainPanel({
                             textOutput("timespan_U")
                             
                             
                           }))),
                
                navbarMenu("More",
                           tabPanel("Sub-Component A"),
                           tabPanel("Sub-Component B")), 
               
                
                
                tags$footer(HTML('
                          <br>
                          <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))




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


