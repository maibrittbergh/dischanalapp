

# Preparation -------------------------------------------------------------

#1. Loading Packages

library("shinythemes")
library(gridExtra)
library('scico')
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
library(zyp)
library(Kendall)
library(zoo)
library(readr)
library(viridisLite)
library(RColorBrewer)
library(sp)
library(rgdal)
library(DT) #make sure you load DT after Shiny
library(dischanalyst)
library(shinycssloaders)
library(shinyWidgets)
library(fontawesome)
library(readr)
library(shinyjs)
library(dischanalyst)


# Load own dataset --------------------------------------------------------

path="/Users/maibrittberghofer/Desktop/Bachelorarbeit/Datafolder/grdc_03_2021/grdc_disc"
Country="DE" #in which country are you interested?
metadata_germany=metadata_grdc(Country, path)

# to avoid mistakes

metadata_repg=metadata_repg(metadata_germany, mark=T)

double=which(metadata_repg$station=="BAD SUELZE")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="BAD TOELZ")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="MITTENWALD")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="PFORZHEIM")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="PLOCHINGEN")

metadata_repg=metadata_repg[-double[1],]
double=which(metadata_repg$station=="STEIN")

metadata_repg=metadata_repg[-double[1],]

data2=metadata_repg
data=grdc_list(data2, path)



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
#MQ_trendset und probd löschen dfMQ
#habe bei min_trend "Kendall" rausgenommen :D und Qmin_trend
