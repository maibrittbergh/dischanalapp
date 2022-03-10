
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



ui = navbarPage(title="Niedrigwasseranalyse für Deutschland", theme = shinytheme("paper"),
                
                


# First Tab ---------------------------------------------------------------
tabPanel(title="Stationsanalyse",
         
         fluidRow(
           column(8, 
                  conditionalPanel(condition="input.plot_tabs!='Anleitung'", 
                                   tabsetPanel(id="ui_tab", 
                                               
                                               tabPanel("Karte", 
                                                        column(12, h4("Wähle eine Station"), shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="800px"),
                                                                                                                    size=3, color="#0080b7"))), 
                                               tabPanel("Tabelle", 
                                                        column(12, h4("Wähle eine Station"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))
                                               ))
                  ),
                  conditionalPanel(condition="input.plot_tabs=='Anleitung'", column(12)
                  )
           ), #Abschließen der linken Spalte mit Tabelle und Map
           
           column(4, tabsetPanel(id="plot_tabs", 
                                 tabPanel("Deskriptive Statistik", 
                                          fluidRow(column(10, 
                                                          # uiOutput("date_slider") Vielleicht statt Jahr?
                                                          
                                                          
                                                          actionButton('help', 'Hilfe'), 
                                                          radioButtons("ts_plot_type", "Darstellung:", choices=c("Zeitreihenanalyse", "Trendanalyse"), 
                                                                       inline=T), #Functions QBoxplot, QBoxploty, Qplot, Qploty 
                                                          
                                                          conditionalPanel(condition="input.ts_plot_type=='Zeitreihenanalyse'", 
                                                                           selectInput("qplot_variety", label="Optionen der Zeitreihenanalyse:",
                                                                                       choices=c("Abflussganglinie",   "jährliche Abflussganglinie", "Boxplot der Messwerte",  "jährlicher Boxplot der Messwerte",   "Plot der Jahreszeiten")) 
                                                                           
                                                                           
                                                                           ,
                                                                           
                                                                           conditionalPanel(condition="input.qplot_variety=='jährlicher Boxplot der Messwerte'",  sliderInput("year", "Jahr: ", 2000, min=1975, max=2015, sep="")),
                                                                           
                                                                           conditionalPanel(condition="input.qplot_variety=='Abflussganglinie'" , checkboxInput("pettitt1", "Pettitt-Test:", value=FALSE)),
                                                                           
                                                                           
                                                                           conditionalPanel(condition="input.qplot_variety=='jährliche Abflussganglinie'",  sliderInput("year2", "Jahr: ", 2000, min=1975, max=2015, sep=""), checkboxInput("hyeardis", label="Hydrologisches Jahr", value=TRUE), 
                                                                                            checkboxInput("pettitt2", "Pettitt-Test", value=FALSE) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season1", "Anfang des Jahresabschnitts:",5,min=01, max=12)),
                                                                           conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  sliderInput("season2", "Ende des Jahresabschnitts:",5,min=01, max=12, ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("ssy", "Startjahr:",2000, min=1999, max=2005 ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",  numericInput("sey", "Endjahr:",2001, min=1999, max=2005 ) ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Plot der Jahreszeiten'",      actionButton("printplot", label="Erstelle Plot")  ),
                                                                           conditionalPanel(condition="input.qplot_variety=='Trendplot'",      renderText({"Loading may take some time. Thank you for your patience."}) ),
                                                                           
                                                                           
                                                                           plotOutput("disch_plot", width = "100%"), 
                                                                           
                                                                           
                                                                           actionButton("cleardata", label="Lösche Darstellungsoptionen"), 
                                                                           actionButton("reset2", "Reset")) , 
                                                          
                                                          
                                                          
                                                          conditionalPanel(condition="input.ts_plot_type=='Trendanalyse'",
                                                                           selectInput("trendpltype", "Optionen der Trendanalyse:", choices=c("Trend der Minimumwerte", "NMxQ-Trend", "Trend der Mittewlwerte")),
                                                                           
                                                                           
                                                                           
                                                                           conditionalPanel( condition="input.trendpltype=='NMxQ-Trend'", 
                                                                                             sliderInput("xVALUE", "X-Value", value=14, min=4, max=90), 
                                                                                             selectInput("season_trend", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))), 
                                                                           
                                                                           
                                                                           #Trend der Mittewlwerte"){
                                                                           
                                                                           
                                                                           
                                                                           conditionalPanel( condition="input.trendpltype=='Trend der Mittelwerte'", 
                                                                                             
                                                                                             selectInput("season_trend_2", "Jahr/Jahreszeit:",choices= c("Jahr", "Winter", "Frühling", "Sommer", "Herbst"))), 
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           plotOutput("trendplot") %>% withSpinner(color="#0dc5c1"), 
                                                                           
                                                                           actionButton("cleardata2", label="Lösche Darstellungsoptionen"), 
                                                                           actionButton("reset2", "Reset"))
                                                          
                                                          
                                                          
                                                          
                                          )) ),
                                 
                                 
                                 tabPanel("Schwellenwertbasiert", 
                                          
                                          
                                          fluidRow(column(10, 
                                                          # uiOutput("date_slider") Vielleicht statt Jahr?
                                                          
                                                          # conditionalPanel(condition="input.qplot_variety=='jährliche Abflussganglinie'", 
                                                          actionButton('helpthres', 'Hilfe'), 
                                                          radioButtons("thres_type", "Grenzwert:", choices=c("Quantilbasiert", "Individueller Wert"), 
                                                                       inline=T), 
                                                          
                                                          
                                                          conditionalPanel(condition="input.thres_type=='Quantilbasiert'", 
                                                                           sliderInput("quantile", label="Quantile", min=0.05, max=1, value=0.3, step=0.05), sliderInput("yearq", "Jahr: ", 2000, min=1975, max=2015, sep="")), 
                                                          
                                                          
                                                          conditionalPanel(condition="input.thres_type=='Individueller Wert'", 
                                                                           sliderInput("value", label="Value", min=0, max=3000, value=150,  sep=""), sliderInput("yearv", "Jahr: ", 2000, min=1975, max=2015, sep="")), 
                                                          
                                                          plotOutput("thresplot", width = "100%"), 
                                                          
                                                          actionButton("cleardata3", label="Lösche Darstellungsoptionen")
                                                          
                                          )) 
                                          
                                          
                                          
                                 ), 
                                 
                                 
                                 
                                 
                                 tabPanel("Anleitung",
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
                  conditionalPanel(condition="input.area_trend!='Anleitung'",
                  
                  tabPanel("Areal Trends and Characteristics in Germany", 
                           column(12, h4("Click to See the Stations Name"), shinycssloaders::withSpinner(leaflet::leafletOutput("datamap", height="800px"),
                                                                                                         size=3, color="#0080b7"))) %>% withSpinner(color="#0dc5c1"), 
                  
                  
                  
                  
           )),
           
         #  conditionalPanel(condition="input.area_trend=='Anleitung'", 
                            
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
                              
                              tabPanel(title="Anleitung",id="Anleitung"
                                       
  
                                       
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
                                                                  sliderInput("range", "Select Timerange:", value=c(1998
                                                                                                                    ,2004), min=min(data$startyear), max=max(data$endyear), sep=""), 
                                                                  actionButton("gostations", "Show Stations")
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                  ))))))),





# Graphics ----------------------------------------------------------------

# conditionalPanel(condition="input.plot_tabs!='Anleitung'", 
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



