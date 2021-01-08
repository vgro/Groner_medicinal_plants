###############################################
# script for RAMAS sensitivity analysis
##
###############################################

library(rgdal)  
library(raster) 
library(sp)
library(tidyr)
library(mptools)
require(reshape2)

# data source
species_name <-'Clivia_miniata'

path<-paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/scen000/RAMAS_metapop/',sep='')
time<-2020:2050
time1<-2019:2050

infiles1<-list.files(path, pattern='.MP')
infiles<-infiles1[c(2,9,12,18)]

# load data mptools 
mp1<-list()
res<-list()
names_list<-list()
i=1
for (j in 1:length(infiles)){
 mp1[[j]] <- (paste(path,infiles[[j]],sep=''))
 res[[j]]<- results(mp=mp1[[j]]) 
 names_list[[i]]<-paste(infiles[[j]])
 i=i+1
  }

dfc<-data.frame(time)
for (k in 1:(length(infiles))){
  dfc<-cbind(dfc,res[[k]]$results[,,'ALL'])
}
dfc
head(dfc)
#rite.csv(df,file=paste(path,parameter,'_sensitivity_analysis.csv',sep=''))

names_list
#2030
site1_low2030<-(dfc[11,2]-(2*dfc[11,3]))*100/32825
site1_high2030<-(dfc[11,6]+(2*dfc[11,7]))*100/32825
site2_low2030<-(dfc[11,10]-(2*dfc[11,11]))*100/32825
site2_high2030<-(dfc[11,14]+(2*dfc[11,15]))*100/32825

#2040
site1_low2040<-(dfc[21,2]-(2*dfc[21,3]))*100/32825
site1_high2040<-(dfc[21,6]+(2*dfc[21,7]))*100/32825
site2_low2040<-(dfc[21,10]-(2*dfc[21,11]))*100/32825
site2_high2040<-(dfc[21,14]+(2*dfc[21,15]))*100/32825

#2050
site1_low2050<-(dfc[31,2]-(2*dfc[31,3]))*100/32825
site1_high2050<-(dfc[31,6]+(2*dfc[31,7]))*100/32825
site2_low2050<-(dfc[31,10]-(2*dfc[31,11]))*100/32825
site2_high2050<-(dfc[31,14]+(2*dfc[31,15]))*100/32825


#############################################################################################
#### END ###     
#############################################################################################

