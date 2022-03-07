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
View(metadata_repg)
View(metadata_germany)
metadata_repg=metadata_repg(metadata_germany)
#unter Umständen doppelte stationen entfernen
double=which(metadata_repg$station=="BAD SUELZE")
data=metadata_repg[-double[1],]
data2=data
data=grdc_list(data, path)

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

View(MQlist)

# MINTRENDLIST ------------------------------------------------------------


mintrendlist=vector(mode="list", length=6)

mintrendlist[[1]]=mintrend_1820_2019
mintrendlist[[2]]=mintrend_1860_2019
mintrendlist[[3]]=mintrend_1900_2019

names(mintrendlist)=c("1820-2019", "1860-2019", "1900-2019", "1940-2019", "1980-2019", "1980-2020")
View(mintrendlist)



# NMxQ --------------------------------------------------------------------

  # 7 -----------------------------------------------------------------------


NMxQlist7=vector(mode="list", length=6)
NMxQlist7[[1]]=nmxq_7_1820_2019
NMxQlist7[[2]]=nmxq_7_1860_2019
NMxQlist7[[3]]=nmxq_7_1900_2019
NMxQlist7[[4]]=nmxq_7_1940_2019
NMxQlist7[[5]]=nmxq_7_1980_2019

View(NMxQlist7)

names(NMxQlist7)=c("1820-2019"
              , "1860-2019", "1900-2019", "1940-2019", "1980-2019")

 # 14 ----------------------------------------------------------------------


NMxQlist14=vector(mode="list", length=6)
NMxQlist14[[1]]=nmxq_14_1820_2019

NMxQlist14[[4]]=nmxq_14_1940_2019
NMxQlist14[[5]]=nmxq_14_1980_2019

names(NMxQlist14)=c("1820-2019", "1940-2019", "1980-2019")



# Periodmeta --------------------------------------------------------------

Periodmeta=vector(mode="list", length=6)
Periodmeta[[1]]=period_1820_2019
Periodmeta[[2]]=period_1820_2019
Periodmeta[[3]]=period_1820_2019
Periodmeta[[4]]=period_1820_2019
Periodmeta[[5]]=period_1820_2019

names(Periodmeta)=c("1820-2019"
                   , "1860-2019", "1900-2019", "1940-2019", "1980-2019")
class(Periodmeta[[1]]$Q95sigld)
View(Periodmeta)
################

citation()
library(dischanalyst)
citation("shiny")
l=length(data$river)
for(i in 1:l){
  stat=which(data$river==data$river[i])
}

uniq=unique(data$river)
riverl=length(uniq) #204
uniqstat=unique(data$station)
stationl=length(uniqstat) #330
area_dist(data)
area_dist=function(metadata){
  metadata=metadata_repg(metadata, mark=T)
  
  
  graph=ggplot(data)+geom_histogram(aes( x=catch_area/1000),bins=20, fill="darkcyan", col="grey")+ylab("Anzahl")+labs(title="Größe der Einzugsgebiete")+xlab(expression('Einzugsgebietsgröße [km'^2*']  x10'^3))
  graph$dat
  return(graph)
  
  
}

hist=hist(data$catch_area, breaks=c(-20000,0,100,1000,10000,300000))
hist$counts
hist$equidist
hist$breaks
hist$counts
counts=c(21, 169, 90, 55)
probs=round((counts/330)*100,2)

probs



length_distribution=function(metadata, type="map"){
  
  nr=nrow(metadata)
  vec=rep("0", nr)
  for ( i in 1: nr){
    vec[i]=sub("  - ", "-", paste(metadata$river[i], "-", metadata$station[i]))
    
  }
  metadata$river_station=vec
  
  st_meta=st_as_sf(metadata, coords=c("longitude","latitude"), crs=4326 )
  
  
  
  
  length_timeseries=st_meta$d_years
  
  metadata$length_timeseries=length_timeseries
  st_meta$length_timeseries=length_timeseries
  
  
  
  
  
  st_meta$startyear=as.character(st_meta$startyear)
  st_meta$endyear=as.character(st_meta$endyear)
  
  
  tmap_mode("view") #view einfügen, damit Hintergrundkarte funktioniert
  
  
  tm=tm_shape(st_meta)+ tm_dots("length_timeseries", title="Length of Timeseries [years]", id="river_station",interactive=T, popup.vars=c(
    "Length of Timeseries"="length_timeseries",
    "Startyear" = "startyear",
    "Endyear"= "endyear"
  ) , palette="YlOrBr")+ tm_scale_bar()+ tm_basemap(c("OpenStreetMap","Esri.WorldImagery"))+
    tm_layout("Length of Timeseries [years]")
  if (type=="map"){
    return(tm)
  }else{
    
    pl=ggplot(metadata)+geom_histogram(aes(y=length_timeseries), col="white")+coord_flip()+
      theme(legend.position = "none")+ labs(y = "Länge der Messreihe [Jahre]", x = "Anzahl",
                                            title =" Längen der Messreihen", cex=5)
    
    return(pl)
  }
  
  
  
  
  
}
library(gridExtra)

grid.arrange(k, a, ncol = 2)


k=length_distribution(data, type="l")
k
a=area_dist(data)
a
citation("dplyr")






#MQ_trendset und probd löschen dfMQ
#habe bei min_trend "Kendall" rausgenommen :D und Qmin_trend
