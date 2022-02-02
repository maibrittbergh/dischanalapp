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
library(shinycssloaders)
library(shinyWidgets)
#install.packages("fontawesome")
library(fontawesome)



# Daten vorbereiten -------------------------------------------------------


#st_grdc=st_as_sf(metadata_repg, coords=c("longitude","latitude"), crs=4326 )

#data=st_grdc

data=metadata_repg[, -c(7,8)]


metadata_repg=metadata_repg(metadata_germany)

metadata_gerrep=metadata_repg(metadata_germany, mark=T)

data=metadata_gerrep


data2= grdc_list(metadata_germany, path)
data3=grdc_list(metadata_rep,path)
  #grdc_list(metadata_germany, path)
#data3=GRDC_list(metadata_germany, path)
data4=metadata_rep


repres=relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
          "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
          "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" , "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")



View(data)






ui = navbarPage(title="Low Flow Analysis in Germany", theme = shinytheme("paper"),
                

# First Tab ---------------------------------------------------------------

                
                
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
                                                                          
                                                                          
                                                                          actionButton('help', 'Help'), 
                                                                          radioButtons("ts_plot_type", "Plot type:", choices=c("Discharge Measurements", "Trend Analysis"), 
                                                                                       inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty 
                                                                          
                                                                          conditionalPanel(condition="input.ts_plot_type=='Discharge Measurements'", 
                                                                                           selectInput("qplot_variety", label="Display Options for Discharge Measurements:",
                                                                                                       choices=c("Discharge Plot",   "annual Discharge Plot", "annual Discharge Boxplot", "Discharge Boxplot",    "Seasonplot")) 
                                                                          
                                                                          
                                                                          ,
                                                                          conditionalPanel(condition="input.qplot_variety=='annual Discharge Boxplot'",  sliderInput("year", "Select Year:", 2000, min=1975, max=2015, sep="")),
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          conditionalPanel(condition="input.qplot_variety=='annual Discharge Plot'",  sliderInput("year2", "Select Year:", 2000, min=1975, max=2015, sep="") ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season1", "Select Begin of the Season:",5,min=01, max=12)),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season2", "Select End of the Season:",5,min=01, max=12, ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("ssy", "Select Startyear:",2000, min=1999, max=2005 ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("sey", "Select Endyear:",2001, min=1999, max=2005 ) ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Seasonplot'",      actionButton("printplot", label="Print Plot")  ),
                                                                          conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),
                                                                          
                                                                 
                                                                        
                                                                          plotOutput("disch_plot", width = "100%"), 
                                                                          
                                                                          actionButton("cleardata", label="Clear Data")) , 
                                                                          
                                                                          
                                                                          
                                                                          conditionalPanel(condition="input.ts_plot_type=='Trend Analysis'",
                                                                                           selectInput("trendtype", "Method to calculate the trend", choices=c( "Linear Model: Least Squares Approach", "Yuepilon-Method: PreWhitening and homogenization of autocorrelation","Yuepilon-Method and Linear Approach")), 
                                                                                           plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"), 
                                                                                           
                                                                                           actionButton("cleardata2", label="Clear Data") )
                                                                                           
                                                                          
                                                                          
                                                                          
                                                          )) ),
                                                 
                                                 
                                                 tabPanel("Threshold-based", 
                                                          
                                                          
                                                          fluidRow(column(10, 
                                                                          # uiOutput("date_slider") Vielleicht statt Jahr?
                                                                          
                                                                          # conditionalPanel(condition="input.qplot_variety=='annual Discharge Plot'", 
                                                                          actionButton('helpthres', 'Help'), 
                                                                          radioButtons("thres_type", "Threshold:", choices=c("Quantile Based", "Choose individual Value"), 
                                                                                       inline=T), 
                                                                          
                                                                          
                                                                          conditionalPanel(condition="input.thres_type=='Quantile Based'", 
                                                                                          sliderInput("quantile", label="Quantile", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Select Year:", 2000, min=1975, max=2015, sep="")), 
                                                                                           
                                                                          
                                                                          conditionalPanel(condition="input.thres_type=='Choose individual Value'", 
                                                                                           sliderInput("value", label="Value", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Select Year:", 2000, min=1975, max=2015, sep="")), 
                                                                          
                                                                          plotOutput("thresplot", width = "100%"), 
                                                                          
                                                                          actionButton("cleardata3", label="Clear Data")
                                                                          
                                                                          )) 
                                                                          
                                                          
                                                          
                                                 ), 
                                                 
                                                 
                                                 
                                                 
                                                 tabPanel("User guide",
                                                          fluidRow(
                                                            column(8,
                                                                   # includeMarkdown('./user_guide/user_guide.rmd') #including MArkdown for Users Guide 
                                                            )
                                                          )
                                                 )
                                                 
                           )))), 
                
                
                
                
                
                

# Second Tab --------------------------------------------------------------

                
                
                
                tabPanel(title="Trend of Minimum Discharge", 
                         
                         fluidRow(
                           column(9, 
        
                                                               
                                                               tabPanel("Areal Trends and Characteristics in Germany", 
                                                                        column(12, h4("Click to See the Stations Name"), shinycssloaders::withSpinner(leaflet::leafletOutput("areamap", height="800px"),
                                                                                                                                    size=3, color="#0080b7"))) %>% withSpinner(color="#0dc5c1"), 
                                                          
                         
                         
                         
                         ),
                         
                         column(3,
                                tabsetPanel(id="area_trend", 
                                            tabPanel("Settings", 
                                                     fluidRow(column(10, 
                                                                     checkboxInput("pettit", "Pettit-Test"),
                                                                     
                                                                     
                                                                     radioButtons("dataset", "Select Dataset", choices=c("Representative Stations only", "All GRDC-Stations in Germany")), 
                                                                     sliderInput("range", "Select Timerange:", value=c(1995,2005), min=min(data$startyear), max=max(data$endyear), sep=""),
                                                                  
                                                               
                                                                                 
                                                      
                                                                     
                                                                     selectInput("trendarea", label="Possible Approaches for area-based evaluation: ",
                                                                                 choices=c("MQ",   "annual Discharge Plot", "annual Discharge Boxplot", "Discharge Boxplot",    "Seasonplot")) ,
                                                                     
                                                                     
                                                                     conditionalPanel(condition="input.ts_plot_type=='Discharge Measurements'", 
                                                                                      selectInput("qplot_variety", label="Display Options for Discharge Measurements:",
                                                                                                  choices=c("Discharge Plot",   "annual Discharge Plot", "annual Discharge Boxplot", "Discharge Boxplot",    "Seasonplot")) 
                                                                                      
                                                                                      
                                                                                      ,
                                                                                      conditionalPanel(condition="input.qplot_variety=='annual Discharge Boxplot'",  sliderInput("year", "Select Year:", 2000, min=1975, max=2015, sep="")),
                                                                                      
                                
                                
                                ))))))
                         
                         
                         )),
                
                
                
                
                
                
                
                

# Third Tab ---------------------------------------------------------------

                
                
                
                
                tabPanel(title="Data set Information", 
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






