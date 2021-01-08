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

path<-paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/sensitivity_analysis/RAMAS_metapop_dispersal/',sep='')
time<-2020:2050
time1<-2019:2050

infiles<-list.files(path, pattern='.MP')
infiles
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

df<-data.frame(time)
for (k in 1:(length(infiles))){
  df<-cbind(df,res[[k]]$results[,,'ALL'])
}
df
head(df)
#write.csv(df,file=paste(path,parameter,'_sensitivity_analysis.csv',sep=''))

names_list
#2030
#site1_low2030<-
  (df[11,2]-(2*df[11,3]))*100/32825
#site1_high2030<-
  (df[11,6]+(2*df[11,7]))*100/32825
#site2_low2030<-
  (df[11,10]-(2*df[11,11]))*100/32825
#site2_high2030<-
  (df[11,14]+(2*df[11,15]))*100/32825

#2040
#site1_low2040<-
  (df[21,2]-(2*df[21,3]))*100/32825
#site1_high2040<-
  (df[21,6]+(2*df[21,7]))*100/32825
#site2_low2040<-
  (df[21,10]-(2*df[21,11]))*100/32825
#site2_high2040<-
  (df[21,14]+(2*df[21,15]))*100/32825

#2050
#site1_low2050<-
  (df[31,2]-(2*df[31,3]))*100/32825
#site1_high2050<-
  (df[31,6]+(2*df[31,7]))*100/32825
#site2_low2050<-
  (df[31,10]-(2*df[31,11]))*100/32825
#site2_high2050<-
  (df[31,14]+(2*df[31,15]))*100/32825


#############################################################################################
#### END ###     
#############################################################################################

