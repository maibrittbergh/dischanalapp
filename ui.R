
library("shinythemes")
library(gridExtra)
install.packages('scico')
library('scico')
install.packages("dichromat")
library("dichromat")
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
library(readr)
library(shinyjs)
meta=data
data=data2
data2=meta


# Daten vorbereiten -------------------------------------------------------




repres=relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
                 "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
                 "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" , "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")



ui = navbarPage(title="Low Flow Analysis in Germany", theme = shinytheme("paper"),
                
                


# First Tab ---------------------------------------------------------------
tabPanel(title="Discharge Map",
         
         fluidRow(
           column(8, 
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
           
           column(4, tabsetPanel(id="plot_tabs", 
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
                                                                           
                                                                           conditionalPanel(condition="input.qplot_variety=='Discharge Plot'" , checkboxInput("pettitt1", "Pettitt-Test", value=FALSE)),
                                                                           
                                                                           
                                                                           conditionalPanel(condition="input.qplot_variety=='annual Discharge Plot'",  sliderInput("year2", "Select Year:", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrological Year", value=TRUE), 
                                                                                            checkboxInput("pettitt2", "Pettitt-Test", value=FALSE) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season1", "Select Begin of the Season:",5,min=01, max=12)),
                                                                           conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  sliderInput("season2", "Select End of the Season:",5,min=01, max=12, ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("ssy", "Select Startyear:",2000, min=1999, max=2005 ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Seasonplot'",  numericInput("sey", "Select Endyear:",2001, min=1999, max=2005 ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Seasonplot'",      actionButton("printplot", label="Print Plot")  ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),
                                                                           
                                                                           
                                                                           plotOutput("disch_plot", width = "100%"), 
                                                                           
                                                                           
                                                                           actionButton("cleardata", label="Clear Data"), 
                                                                           actionButton("reset2", "Reset")) , 
                                                          
                                                          
                                                          
                                                          conditionalPanel(condition="input.ts_plot_type=='Trend Analysis'",
                                                                           selectInput("trendpltype", "Type of plot", choices=c("Trend of minimum Values", "NMxQ-Trend", "Trend of Mean Values")),
                                                                           
                                                                           
                                                                           
                                                                           conditionalPanel( condition="input.trendpltype=='NMxQ-Trend'", 
                                                                                             sliderInput("xVALUE", "X-Value", value=14, min=4, max=90), 
                                                                                             selectInput("season_trend", "Choose a Season",choices= c("Year", "Winter", "Spring", "Summer", "Autumn"))), 
                                                                           
                                                                           
                                                                           #Trend of Mean Values"){
                                                                           
                                                                           
                                                                           
                                                                           conditionalPanel( condition="input.trendpltype=='Trend of Mean Values'", 
                                                                                             
                                                                                             selectInput("season_trend_2", "Choose a Season",choices= c("Year", "Winter", "Spring", "Summer", "Autumn"))), 
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"), 
                                                                           
                                                                           actionButton("cleardata2", label="Clear Data"), 
                                                                           actionButton("reset2", "Reset"))
                                                          
                                                          
                                                          
                                                          
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
    
         id = "side-panel",
         
         fluidRow(
           column(9, 
                  conditionalPanel(condition="input.area_trend!='User Guide'",
                  
                  tabPanel("Areal Trends and Characteristics in Germany", 
                           column(12, h4("Click to See the Stations Name"), shinycssloaders::withSpinner(leaflet::leafletOutput("datamap", height="800px"),
                                                                                                         size=3, color="#0080b7"))) %>% withSpinner(color="#0dc5c1"), 
                  
                  
                  
                  
           )),
           
         #  conditionalPanel(condition="input.area_trend=='User Guide'", 
                            
                          #  includeMarkdown()
                         #   ),
           
           
           
           column(3,
                  tabsetPanel(id="area_trend", 
                              tabPanel("Map Settings", 
                                       fluidRow(column(10, 
                                                       
                                                       
                                                       
                                      
                                                       
                                                       
                                                       selectInput("trendtype2", label="Select Approach for area-based evaluation: ",
                                                                   choices=c( "MQ - Mean Discharge Trend","NMxQ", "Low Flow Period")) ,
                                                       
                                                       radioButtons("dataset", "Update Map, print Stations within Timerange for:", choices=c("All GRDC-Stations in Germany","Representative Stations only")), 
                                                       
                                                       selectInput("timerange2", "Select Timerange:",    choices=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019")), 
                                                       #"1980-2020")),
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       #MQ-Mean Discharge Trend
                                                       
                                                       conditionalPanel(condition="input.trendtype2=='MQ - Mean Discharge Trend'", 
                                                                        selectInput("seasonmq", label="Select the Season:",
                                                                                    choices=c("Spring",   "Summer", "Autumn", "Winter", 
                                                                                              "Year")) , 
                                                                        
                                                                        selectInput("trendtypemq", label="Select Method to calculate the Trend:",
                                                                                    choices=c( "Linear Model: Least Squares Approach", "Yuepilon-Method: PreWhitening and homogenization of autocorrelation", "Significance of Zyp-Trend")),
                                                                        actionButton("go", "Click to calculate results")), 
                                                       
                                                       
                                                       #NMxQ                
                                                       
                                                       
                                                       conditionalPanel(condition="input.trendtype2=='NMxQ'", 
                                                                        selectInput("xval", label="Select X-Value:",
                                                                                    choices=c("7","14", "30","60")) , 
                                                                        
                                                                        selectInput("seasonmq2", label="Select the Season:",
                                                                                    choices=c("Spring",   "Summer", "Autumn", "Winter", 
                                                                                              "Year")),
                                                                        
                                                                        selectInput("trendtypemq2", label="Select Method to calculate the Trend:",
                                                                                    choices=c( "Linear Model: Least Squares Approach", "Yuepilon-Method: PreWhitening and homogenization of autocorrelation", "Significance of Zyp-Trend")),
                                                                        actionButton("go_NMxQ", "Click to (re)calculate results")),
                                                       
                                                       
      
                                                       
                                                       
                                                       
                                                       
                                                       ###Periodmeta 
                                                       
                                                       
                                                       conditionalPanel(condition="input.trendtype2=='Low Flow Period'", 
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        selectInput("periodway", "Choose Value:", choices=c("Length of Maximum Period under Value","Sum of Days under Value")), 
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        selectInput("trendtypeperiod", label="Select Method to calculate the Trend:",
                                                                                    choices=c( "Linear Model: Least Squares Approach", "Yuepilon-Method: PreWhitening and homogenization of autocorrelation", "Significance of Zyp-Trend")),
                                                                        
                                                                        selectInput("quantiles", label="Quantile [%]:",
                                                                                    choices=c("70","75", "80","85", "90", "90", "95")) , 
                                                                        actionButton("go_2", "Click to (re)calculate results"))
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       
                                       ))), 
                              
                              tabPanel(title="User Guide",id="User Guide"
                                       
  
                                       
                                       )
                              
                              
                              )))
         
         
),


                
           
              



                
                
              
                
                

# Third Page --------------------------------------------------------------



navbarMenu(title="Dataset Information",
           
           

# MAP ---------------------------------------------------------------------

           
           tabPanel("Info Map", 
                    
                    
                    
                    
                    
                    fluidRow(
                      column(7, 
                             
                             
                             tabPanel("StationDistribution", 
                                      column(12, h4("Click to See the Stations Information"), shinycssloaders::withSpinner(leaflet::leafletOutput("stationmap", height="800px"),
                                                                                                                           size=3, color="#0080b7"))) , 
                             
                             
                             
                             
                      ),
                      
                      column(5,
                             tabsetPanel(id="data_dist", 
                                         tabPanel("Settings", 
                                                  fluidRow(column(10, 
                                                                  
                                                                  
                                                                  
                                                                  radioButtons("dataselect", "Select Dataset", choices=c("All GRDC-Stations in Germany","Representative Stations only")), 
                                                                  sliderInput("range", "Select Timerange:", value=c(2000
                                                                                                                    ,2001), min=min(data$startyear), max=max(data$endyear), sep="")
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                  ))))))),





# Graphics ----------------------------------------------------------------

# conditionalPanel(condition="input.plot_tabs!='User guide'", 
#tabsetPanel(id="ui_tab", 
           tabPanel("Data Distribution", 
                    
                    
                    fluidRow(
                      column(4, 
                             
                             selectInput("ddgraph", "Data Distribution Graph", choices=c(
                               #"Length: Timeseries of Discharge Data",
                               "Compare Discharge Measurements",  "Area Distribution" )), 
                             
                             
                            # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'",  radioButtons("densl", "Presentation", choices=c("Density Plot","Colour Map"))),
                             conditionalPanel(condition= "input.ddgraph=='Compare Discharge Measurements'", sliderInput("yeatise", " X-Axis-Resolution (Timeframe):", value=c(1950, 2000), min=1820, max=2020, sep="") , sliderInput("frametise", "Y-Axis Resolution:", value=c(0, 3100), min=0, max=7000, sep="") )
                                              
                                             
                             
                                                
                                              
                             ),
                      column(8, 
                            # conditionalPanel(condition= "input.ddgraph=='Length: Timeseries of Discharge Data'", 
                             #conditionalPanel(condition="input.densl=='Density Plot'", plotOutput("distplot", width = "100%", height=400) %>% withSpinner(color="#0dc5c1")), 
                             #conditionalPanel(condition="input.densl=='Colour Map'", tmapOutput("tmap", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1"))) , 
                             
                             conditionalPanel(condition="input.ddgraph=='Compare Discharge Measurements'",  plotOutput("tisepl", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1")) , 
                             
                             conditionalPanel(condition="input.ddgraph=='Area Distribution'",  plotOutput("areapl", width = "100%", height = 700) %>% withSpinner(color="#0dc5c1")) 
                      
                     
                    )
                    
                    ))), 











                
                





                
                

# Third Tab ---------------------------------------------------------------

                
                
                
            
                
              
                
                
                
                tags$footer(HTML('
                          <br>
                          <br>
                          <p>Author: Mai-Britt Berghöfer <br>
                          <a href="mailto:berghoefer@uni-potsdam.de">berghoefer@uni-potsdam.de</a></p>'), align = "center"))



