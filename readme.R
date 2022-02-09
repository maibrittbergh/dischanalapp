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




# readmedischanalyst ------------------------------------------------------

#to be able to run the functions of dischanalyst on your computer

# Library Packages --------------------------------------------------------


library(Kendall)
library(dischanalyst)





# Load in data ------------------------------------------------------------
#enter path: where did you save GRDC-Dataset (path to grdc_disc)
path="/Users/maibrittberghofer/Desktop/Bachelorarbeit/Datafolder/grdc_03_2021/grdc_disc"
Country="DE" #in which country are you interested?
metadata_germany=metadata_grdc(Country, path)


# Load in datset of interest (specific river as well as station )
rivername="MOSELLE RIVER" #rivername must be equal like rivername in metadata
station= "COCHEM" #stationname must be equal like stationname  in metadata
mosel=grdc_readr(metadata_germany, rivername, path )


#representative level for germany

metadata_repg=metadata_repg(metadata_germany,mark=T)
#unter Umständen doppelte stationen entfernen
double=which(metadata_repg$station=="BAD SUELZE")
metadata_repg=metadata_repg[-double[1],]

data=grdc_list(metadata_repg, path)

# Creating List including Data --------------------------------------------
GRDC_list=function(metadata, path){
  
  length=nrow(metadata)
  grdc_list=vector(mode = "list", length = length)
  
  
  
  for ( i in 1:length){
    
    data=grdc_readr(metadata,   metadata$river[i]    , path)
    station=metadata$station[i]
    nbr=which(names(data)== station)
    val=data[[nbr]]
    
    
    
    grdc_list[[i]]=val
    names(grdc_list)=metadata$station
    
  }
  
  
  return(grdc_list)
}


#when using easier to have list of one river GRDC Reader than of all measurements







#Sollte man das noch in GRDC - list integrieren?:

#if (any(val[,2]==0)){
#  
#  zero=which(val[,2]==0)
#  l=length(zero)
#  for (i in 1:l){
#    val[zero[i],2]=NA
#  }
#  
#}




