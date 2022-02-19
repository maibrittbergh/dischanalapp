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
data=metadata_repg[-double[1],]

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


# creating Lists of Secondplot


# MQLIST ------------------------------------------------------------------



MQlist=vector(mode="list", length=6)
MQlist[[1]]=new_MQ_1820_2019
MQlist[[2]]=metaMQ_1860_2019
MQlist[[3]]=metaMQ_1900_2019
MQlist[[4]]=metaMQ_1940_2019


names(MQlist)= c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019", "1980-2020")



# MINTRENDLIST ------------------------------------------------------------


mintrendlist=vector(mode="list", length=6)

mintrendlist[[1]]=mintrend_1820_2019
mintrendlist[[2]]=mintrend_1860_2019
mintrendlist[[3]]=mintrend_1900_2019
View(mintrendlist)
names(mintrendlist)=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019", "1980-2020")
mintrendlist



# NMxQ --------------------------------------------------------------------

  # 7 -----------------------------------------------------------------------


NMxQlist7=vector(mode="list", length=6)
NMxQlist7[[1]]=nmxq_7_1820_2019
NMxQlist7[[2]]=nmxq_7_1860_2019
NMxQlist7[[3]]=nmxq_7_1900_2019
NMxQlist7[[4]]=nmxq_7_1940_2019
NMxQlist7[[5]]=nmxq_7_1980_2019

names(NMxQlist7)=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019", "1980-2020")
View(NMxQlist7)



 # 14 ----------------------------------------------------------------------


NMxQlist14=vector(mode="list", length=6)
NMxQlist14[[1]]=nmxq_14_1820_2019
NMxQlist14[[2]]=nmxq_14_1860_2019
NMxQlist14[[3]]=nmxq_14_1900_2019
NMxQlist14[[4]]=nmxq_14_1940_2019
NMxQlist14[[5]]=nmxq_14_1980_2019

names(NMxQlist14)=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019", "1980-2020")


