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
                           tabPanel("Sub-Component B")))



