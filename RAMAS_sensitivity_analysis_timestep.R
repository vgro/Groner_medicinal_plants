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
parameter<-'timestep/'

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
write.csv(df,file<-paste(path,parameter,'_sensitivity_analysis.csv',sep=''))

names_list
#2030
#site1_t1_low2030<-
(df[11,2]-(2*df[11,3]))*100/(32825)
#site1_t1_lup030<-
(df[11,6]+(2*df[11,7]))*100/(32825)
#site2_t1low2030<-
(df[11,10]-(2*df[11,11]))*100/(32825)
#site2_t1_high2030<-
  (df[11,14]+(2*df[11,15]))*100/(32825)

#site1_t3_low2030<-
  (df[11,18]-(2*df[11,19]))*100/(32825)
#site1_t3_high2030<-
  (df[11,22]+(2*df[11,23]))*100/(32825)
#site2_t3_low2030<-
  (df[11,26]-(2*df[11,27]))*100/(32825)
#site2_t3_high2030<-
  (df[11,30]+(2*df[11,31]))*100/(32825)


#2040
#site1_t1_low2040<-
(df[21,2]-(2*df[21,3]))*100/(32825)
#site1_t1_lup2040<-
(df[21,6]+(2*df[21,7]))*100/(32825)
#site2_t1low2040<-
(df[21,10]-(2*df[21,11]))*100/(32825)
#site2_t1_high2040<-
(df[21,14]+(2*df[21,15]))*100/(32825)

#site1_t3_low2040<-
(df[21,18]-(2*df[21,19]))*100/(32825)
#site1_t3_high2040<-
(df[21,22]+(2*df[21,23]))*100/(32825)
#site2_t3_low2040<-
(df[21,26]-(2*df[21,27]))*100/(32825)
#site2_t3_high2040<-
(df[21,30]+(2*df[21,31]))*100/(32825)

#2050
#site1_t1_low2050<-
(df[31,2]-(2*df[31,3]))*100/(32825)
#site1_t1_lup2050<-
(df[31,6]+(2*df[31,7]))*100/(32825)
#site2_t1low2050<-
(df[31,10]-(2*df[31,11]))*100/(32825)
#site2_t1_high2050<-
(df[31,14]+(2*df[31,15]))*100/(32825)

#site1_t3_low2050<-
(df[31,18]-(2*df[31,19]))*100/(32825)
#site1_t3_high2050<-
(df[31,22]+(2*df[31,23]))*100/(32825)
#site2_t3_low2050<-
(df[31,26]-(2*df[31,27]))*100/(32825)
#site2_t3_high2050<-
(df[31,30]+(2*df[31,31]))*100/(32825)

#plot

c1 <- rainbow(10)
c2 <- rainbow(10, alpha=0.2)
c3 <- rainbow(10, v=0.7)

#site1
plot(time1,c(100,df$mean),ylim=c(0,100),col='white', ylab='abundance [%]',xlab='year')
legend("topright", fill = c(c2[[1]],c2[[5]],c2[[7]]), legend = c('1 year','3 years','5 years'),box.col='white',inset=0.01)

polygon(c(time1, rev(time1)),c(c(100,(df[,2]-(2*df[,3]))*100/(32825)), rev(c(100,(df[,6]-(2*df[,7]))*100/(32825)))), col=c2[[1]], border =NA) 
polygon(c(time1, rev(time1)),c(c(100,(df[,18]-(2*df[,19]))*100/(32825)), rev(c(100,(df[,22]-(2*df[,23]))*100/(32825)))), col=c2[[5]], border =NA) 
polygon(c(time1, rev(time1)),c(c(100,(dfc[,2]-(2*dfc[,3]))*100/(32825)), rev(c(100,(dfc[,6]-(2*dfc[,7]))*100/(32825)))), col=c2[[7]], border =NA) 

# plot

#site2
plot(time1,c(100,df$mean),ylim=c(0,100),col='white', ylab='abundance [%]',xlab='year')
legend("topright", fill = c(c2[[1]],c2[[5]],c2[[7]]), legend = c('1 year','3 years','5 years'),box.col='white',inset=0.01)

polygon(c(time1, rev(time1)),c(c(100,(df[,10]-(2*df[,11]))*100/(32825)), rev(c(100,(df[,14]-(2*df[,15]))*100/(32825)))), col=c2[[1]], border =NA) 
polygon(c(time1, rev(time1)),c(c(100,(df[,26]-(2*df[,27]))*100/(32825)), rev(c(100,(df[,30]-(2*df[,31]))*100/(32825)))), col=c2[[5]], border =NA) 
polygon(c(time1, rev(time1)),c(c(100,(dfc[,10]-(2*dfc[,11]))*100/(32825)), rev(c(100,(dfc[,14]-(2*dfc[,15]))*100/(32825)))), col=c2[[7]], border =NA) 

