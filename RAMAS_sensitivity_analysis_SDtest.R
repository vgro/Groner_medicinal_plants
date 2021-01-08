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
parameter<-'SDtest/'

path<-paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/sensitivity_analysis/RAMAS_metapop_',parameter,sep='')
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
#write.csv(df,file<-paste(path,parameter,'_sensitivity_analysis.csv',sep=''))

names_list
#2030
#site1_l_3015<-
(df[11,2]-(2*df[11,3]))*100/(32825)
#site1_l_4020<-
(df[11,6]-(2*df[11,7]))*100/(32825)
#site1_u_3015<-
(df[11,10]+(2*df[11,11]))*100/(32825)
#site1_u_4020<-
  (df[11,14]+(2*df[11,15]))*100/(32825)

#site2_l_3015<-
  (df[11,18]-(2*df[11,19]))*100/(32825)
#site2_l_4020<-
  (df[11,22]-(2*df[11,23]))*100/(32825)
#site2_h_3015<-
  (df[11,26]+(2*df[11,27]))*100/(32825)
#site2_h_4020<-
  (df[11,30]+(2*df[11,31]))*100/(32825)

#2040
#site1_l_3015<-
(df[21,2]-(2*df[21,3]))*100/(32825)
#site1_l_4020<-
(df[21,6]-(2*df[21,7]))*100/(32825)
#site1_u_3015<-
(df[21,10]+(2*df[21,11]))*100/(32825)
#site1_u_4020<-
(df[21,14]+(2*df[21,15]))*100/(32825)

#site2_l_3015<-
(df[21,18]-(2*df[21,19]))*100/(32825)
#site2_l_4020<-
(df[21,22]-(2*df[21,23]))*100/(32825)
#site2_h_3015<-
(df[21,26]+(2*df[21,27]))*100/(32825)
#site2_h_4020<-
(df[21,30]+(2*df[21,31]))*100/(32825)

#2050
#site1_l_3015<-
(df[31,2]-(2*df[31,3]))*100/(32825)
#site1_l_4020<-
(df[31,6]-(2*df[31,7]))*100/(32825)
#site1_u_3015<-
(df[31,10]+(2*df[31,11]))*100/(32825)
#site1_u_4020<-
(df[31,14]+(2*df[31,15]))*100/(32825)

#site2_l_3015<-
(df[31,18]-(2*df[31,19]))*100/(32825)
#site2_l_4020<-
(df[31,22]-(2*df[31,23]))*100/(32825)
#site2_h_3015<-
(df[31,26]+(2*df[31,27]))*100/(32825)
#site2_h_4020<-
(df[31,30]+(2*df[31,31]))*100/(32825)