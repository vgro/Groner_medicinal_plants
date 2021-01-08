###############################################
# script to read .MP output 
###############################################

library(rgdal)  
library(raster) 
library(sp)
library(tidyr)
library(mptools)
require(ggplot2)
require(reshape2)

# inputs and parameters
path2<-paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/', sep='')
species_name <-'Clivia_miniata'
LU_scen<-c('scen000','scen002')
models<-c("CanESM2",'HadGEM2-ES',"MPI-ESM-MR","MRI-CGCM3")
scenarios<-c('rcp26','rcp85')
group=c('l','u','m')
harvest<-c('','_ha','_hj')
sites<-c(1,2)
time<-2020:2050
time1<-2019:2050

# read baseline output and write data frame for means 2030,2040,2050
mp1_b<-list()
res1_b<-list()
df_b<-list()
df_mean_b<-list()
j=1

for (s in 1:length(sites)){
  for (g in 1:length(group)){
    print(paste('baseline',sites[[s]],group[[g]],sep=' '))
    mp1_b[[j]] <- (paste(path2,species_name,'_baseline_s',sites[[s]],'_',group[[g]],'.MP',sep=''))
    res1_b[[j]] <- results(mp=mp1_b[[j]])
    
    df_b[[j]]<-data.frame(time,res1_b[[j]]$results[,,'ALL'])
    df_mean_b[[j]]<-data.frame(matrix(ncol=1,nrow=3))
    df_mean_b[[j]][1,]<-df_b[[j]][11,2]
    df_mean_b[[j]][2,]<-df_b[[j]][21,2]
    df_mean_b[[j]][3,]<-df_b[[j]][31,2]
    names(df_b[[j]])<-c(paste('baseline_s',s,'_',group[[g]],sep=''),'mean','sd','min','max')
    names(df_mean_b[[j]])<-paste('baseline_s',s,'_',group[[g]],sep='')
    j=j+1
  }
}

df_mean_b1<-as.data.frame(df_mean_b)

# harvest only
mp1_h<-list()
res1_h<-list()
df_h<-list()
df_mean_h<-list()
df_hazard_h<-list()
l=1

for (s in 1:length(sites)){
  for (g in 1:length(group)){
    for (h in 2:3){
      print(paste('harvestonly',sites[[s]],group[[g]],sep=' '))
      mp1_h[[l]] <- (paste(path2,species_name,'_baseline_s',sites[[s]],'_',group[[g]],harvest[[h]],'.MP',sep=''))
      res1_h[[l]] <- results(mp=mp1_h[[l]])
                           
      df_h[[l]]<-data.frame(time,res1_h[[l]]$results[,,'ALL'])
      df_mean_h[[l]]<-data.frame(matrix(ncol=1,nrow=3))
      df_mean_h[[l]][1,]<-df_h[[l]][11,2]
      df_mean_h[[l]][2,]<-df_h[[l]][21,2]
      df_mean_h[[l]][3,]<-df_h[[l]][31,2]
      names(df_h[[l]])<-c(paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep=''),'mean','sd','min','max')
      names(df_mean_h[[l]])<-paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep='')
                           
      if(s==1){
          df_hazard_h[[l]]<-data.frame(matrix(ncol=1,nrow=3))
          df_hazard_h[[l]][1,]<-df_h[[l]][11,2]/df_mean_b1[1,g]
          df_hazard_h[[l]][2,]<-df_h[[l]][21,2]/df_mean_b1[2,g]
          df_hazard_h[[l]][3,]<-df_h[[l]][31,2]/df_mean_b1[3,g]
          names(df_hazard_h[[l]])<-paste('harvestonly_s',s,'_',group[[g]],harvest[[h]],sep='')      
      }
        
      if(s==2){
          df_hazard_h[[l]]<-data.frame(matrix(ncol=1,nrow=3))
          df_hazard_h[[l]][1,]<-df_h[[l]][11,2]/df_mean_b1[1,g+3]
          df_hazard_h[[l]][2,]<-df_h[[l]][21,2]/df_mean_b1[2,g+3]
          df_hazard_h[[l]][3,]<-df_h[[l]][31,2]/df_mean_b1[3,g+3]
          names(df_hazard_h[[l]])<-paste('harvestonly_s',s,'_',group[[g]],harvest[[h]],sep='')      
      }
        
      l=l+1
    }
  }
}

df_hazard_h
df_hazard_h1<-as.data.frame(df_hazard_h)

# read scenario output and write data frame for means 2030,2040,2050
mp1<-list()
res1<-list()
df<-list()
df_mean<-list()
df_hazard<-list()
i=1

# load data mptools 
for ( b in 1: length(scenarios)){
 for (c in 1: length(LU_scen)){
   for (a in 1:length(models)){
     for (s in 1:length(sites)){
       for (g in 1:length(group)){
         for (h in 1:length(harvest)){
           
          print(paste(LU_scen[[c]],models[[a]],scenarios[[b]],s,group[[g]],harvest[[h]],sep=' '))
          mp1[[i]] <- (paste(path2,LU_scen[[c]],'/RAMAS_metapop/',species_name,'_',LU_scen[[c]],'_',models[[a]],'_',scenarios[[b]],'_s',s,'_',group[[g]],harvest[[h]],'.MP',sep=''))
          res1[[i]] <- results(mp=mp1[[i]])
          
          df[[i]]<-data.frame(time,res1[[i]]$results[,,'ALL'])
          df_mean[[i]]<-data.frame(matrix(ncol=1,nrow=3))
          df_mean[[i]][1,]<-df[[i]][11,2]
          df_mean[[i]][2,]<-df[[i]][21,2]
          df_mean[[i]][3,]<-df[[i]][31,2]
          names(df[[i]])<-c(paste(LU_scen[[c]],'_',models[[a]],'_',scenarios[[b]],'_s',s,'_',group[[g]],harvest[[h]],sep=''),'mean','sd','min','max')
          names(df_mean[[i]])<-paste(LU_scen[[c]],'_',models[[a]],'_',scenarios[[b]],'_s',s,'_',group[[g]],harvest[[h]],sep='')
          
         if (s==1){
          df_hazard[[i]]<-data.frame(matrix(ncol=1,nrow=3))
          df_hazard[[i]][1,]<-df[[i]][11,2]/df_mean_b1[1,g]
          df_hazard[[i]][2,]<-df[[i]][21,2]/df_mean_b1[2,g]
          df_hazard[[i]][3,]<-df[[i]][31,2]/df_mean_b1[3,g]
          names(df_hazard[[i]])<-paste(LU_scen[[c]],'_',models[[a]],'_',scenarios[[b]],'_s',s,'_',group[[g]],harvest[[h]],sep='')
         }
          if (s==2){
            df_hazard[[i]]<-data.frame(matrix(ncol=1,nrow=3))
            df_hazard[[i]][1,]<-df[[i]][11,2]/df_mean_b1[1,g+3]
            df_hazard[[i]][2,]<-df[[i]][21,2]/df_mean_b1[2,g+3]
            df_hazard[[i]][3,]<-df[[i]][31,2]/df_mean_b1[3,g+3]
            names(df_hazard[[i]])<-paste(LU_scen[[c]],'_',models[[a]],'_',scenarios[[b]],'_s',s,'_',group[[g]],harvest[[h]],sep='')
          }
          i=i+1
          }
       }
      }
    }
  }
}

head(df_hazard)
df_hazard1<-as.data.frame(df_hazard)
df_hazard1

write.csv(df,file='C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/RAMAS_output_all.csv')
write.csv(df_mean,file='C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/RAMAS_output_mean.csv')
write.csv(df_hazard,file='C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/RAMAS_output_hazard.csv')

######################
# LU only
mp1_l<-list()
res1_l<-list()
df_l<-list()
df_mean_l<-list()
df_hazard_LU<-list()
l=1

for (s in 1:length(sites)){
  for (g in 1:length(group)){
    for (h in 1:length(harvest)){
    print(paste('LUonly',sites[[s]],group[[g]],sep=' '))
    mp1_l[[l]] <- (paste(path2,'scen002/RAMAS_metapop/',species_name,'_scen002_LUonly_s',sites[[s]],'_',group[[g]],harvest[[h]],'.MP',sep=''))
    res1_l[[l]] <- results(mp=mp1_l[[l]])
    
    df_l[[l]]<-data.frame(time,res1_l[[l]]$results[,,'ALL'])
    df_mean_l[[l]]<-data.frame(matrix(ncol=1,nrow=3))
    df_mean_l[[l]][1,]<-df_l[[l]][11,2]
    df_mean_l[[l]][2,]<-df_l[[l]][21,2]
    df_mean_l[[l]][3,]<-df_l[[l]][31,2]
    names(df_l[[l]])<-c(paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep=''),'mean','sd','min','max')
    names(df_mean_l[[l]])<-paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep='')
    
    if(s==1){
    df_hazard_LU[[l]]<-data.frame(matrix(ncol=1,nrow=3))
    df_hazard_LU[[l]][1,]<-df_l[[l]][11,2]/df_mean_b1[1,g]
    df_hazard_LU[[l]][2,]<-df_l[[l]][21,2]/df_mean_b1[2,g]
    df_hazard_LU[[l]][3,]<-df_l[[l]][31,2]/df_mean_b1[3,g]
    names(df_hazard_LU[[l]])<-paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep='')      
    }
    if(s==2){
      df_hazard_LU[[l]]<-data.frame(matrix(ncol=1,nrow=3))
      df_hazard_LU[[l]][1,]<-df_l[[l]][11,2]/df_mean_b1[1,g+3]
      df_hazard_LU[[l]][2,]<-df_l[[l]][21,2]/df_mean_b1[2,g+3]
      df_hazard_LU[[l]][3,]<-df_l[[l]][31,2]/df_mean_b1[3,g+3]
      names(df_hazard_LU[[l]])<-paste('LUonly_s',s,'_',group[[g]],harvest[[h]],sep='')      
    }
    l=l+1
    }
  }
}
df_hazard_LU
df_hazard_LU1<-as.data.frame(df_hazard_LU)

#######

# arrange data in groups
# no harvest
LC_2030_s1<-rep(c(df_hazard_LU1[1,1],df_hazard_LU1[1,4],df_hazard_LU1[1,7]),time=4) #check numbrs
LC_2040_s1<-rep(c(df_hazard_LU1[2,1],df_hazard_LU1[2,4],df_hazard_LU1[2,7]),time=4)
LC_2050_s1<-rep(c(df_hazard_LU1[3,1],df_hazard_LU1[3,4],df_hazard_LU1[3,7]),time=4)

LC_2030_s2<-rep(c(df_hazard_LU1[1,10],df_hazard_LU1[1,13],df_hazard_LU1[1,16]),time=4)
LC_2040_s2<-rep(c(df_hazard_LU1[2,10],df_hazard_LU1[2,13],df_hazard_LU1[2,16]),time=4)
LC_2050_s2<-rep(c(df_hazard_LU1[3,10],df_hazard_LU1[3,13],df_hazard_LU1[3,16]),time=4)


CC_2030_rcp26_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_m[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_u[[1]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u[[1]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u[[1]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u[[1]])
                 
CC_2030_rcp85_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_m[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_u[[1]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u[[1]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u[[1]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u[[1]])

CC_2040_rcp26_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_m[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_u[[2]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u[[2]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u[[2]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u[[2]])

CC_2040_rcp85_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_m[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_u[[2]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u[[2]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u[[2]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u[[2]])

CC_2050_rcp26_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_m[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_u[[3]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u[[3]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u[[3]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u[[3]])

CC_2050_rcp85_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_m[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_u[[3]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u[[3]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u[[3]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u[[3]])

CCLC_2030_rcp26_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_m[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_u[[1]],
                 df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u[[1]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u[[1]],
                 df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u[[1]])

CCLC_2030_rcp85_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_m[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_u[[1]],
                 df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u[[1]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u[[1]],
                 df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u[[1]])

CCLC_2040_rcp26_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_m[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_u[[2]],
                 df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u[[2]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u[[2]],
                 df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u[[2]])

CCLC_2040_rcp85_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_m[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_u[[2]],
                 df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u[[2]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u[[2]],
                 df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u[[2]])

CCLC_2050_rcp26_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_m[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_u[[3]],
                 df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u[[3]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u[[3]],
                 df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u[[3]])

CCLC_2050_rcp85_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_m[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_u[[3]],
                 df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u[[3]],
                 df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u[[3]],
                 df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u[[3]])
#site2
CC_2030_rcp26_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_m[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_u[[1]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u[[1]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u[[1]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u[[1]])

CC_2030_rcp85_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_m[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_u[[1]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u[[1]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u[[1]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u[[1]])

CC_2040_rcp26_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_m[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_u[[2]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u[[2]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u[[2]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u[[2]])

CC_2040_rcp85_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_m[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_u[[2]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u[[2]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u[[2]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u[[2]])

CC_2050_rcp26_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_m[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_u[[3]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u[[3]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u[[3]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u[[3]])

CC_2050_rcp85_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_m[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_u[[3]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u[[3]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u[[3]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u[[3]])

CCLC_2030_rcp26_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_m[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_u[[1]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u[[1]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u[[1]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u[[1]])

CCLC_2030_rcp85_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_m[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_u[[1]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u[[1]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u[[1]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u[[1]])

CCLC_2040_rcp26_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_m[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_u[[2]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u[[2]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u[[2]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u[[2]])

CCLC_2040_rcp85_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_m[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_u[[2]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u[[2]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u[[2]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u[[2]])

CCLC_2050_rcp26_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_m[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_u[[3]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u[[3]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u[[3]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u[[3]])

CCLC_2050_rcp85_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_m[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_u[[3]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u[[3]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u[[3]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u[[3]])


# harvest juveiles
LC_2030_hj_s1<-rep(c(df_hazard_LU1[1,3],df_hazard_LU1[1,6],df_hazard_LU1[1,9]),time=4)
LC_2040_hj_s1<-rep(c(df_hazard_LU1[2,3],df_hazard_LU1[2,6],df_hazard_LU1[2,9]),time=4)
LC_2050_hj_s1<-rep(c(df_hazard_LU1[3,3],df_hazard_LU1[3,6],df_hazard_LU1[3,9]),time=4)

LC_2030_hj_s2<-rep(c(df_hazard_LU1[1,12],df_hazard_LU1[1,15],df_hazard_LU1[1,18]),time=4) # check numbers
LC_2040_hj_s2<-rep(c(df_hazard_LU1[2,12],df_hazard_LU1[2,15],df_hazard_LU1[2,18]),time=4)
LC_2050_hj_s2<-rep(c(df_hazard_LU1[3,12],df_hazard_LU1[3,15],df_hazard_LU1[3,18]),time=4)

H_2030_hj_s1<-rep(c(df_hazard_h1[1,2],df_hazard_h1[1,4],df_hazard_h1[1,6]),time=4)
H_2040_hj_s1<-rep(c(df_hazard_h1[2,2],df_hazard_h1[2,4],df_hazard_h1[2,6]),time=4)
H_2050_hj_s1<-rep(c(df_hazard_h1[3,2],df_hazard_h1[3,4],df_hazard_h1[3,6]),time=4)

H_2030_hj_s2<-rep(c(df_hazard_h1[1,8],df_hazard_h1[1,10],df_hazard_h1[1,12]),time=4) # check numbers
H_2040_hj_s2<-rep(c(df_hazard_h1[2,8],df_hazard_h1[2,10],df_hazard_h1[2,12]),time=4)
H_2050_hj_s2<-rep(c(df_hazard_h1[3,8],df_hazard_h1[3,10],df_hazard_h1[3,12]),time=4)

CC_2030_rcp26_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_hj[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_m_hj[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_u_hj[[1]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_hj[[1]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_hj[[1]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_hj[[1]])

CC_2030_rcp85_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_hj[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_m_hj[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_u_hj[[1]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_hj[[1]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_hj[[1]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_hj[[1]])

CC_2040_rcp26_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_hj[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_m_hj[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_u_hj[[2]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_hj[[2]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_hj[[2]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_hj[[2]])

CC_2040_rcp85_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_hj[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_m_hj[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_u_hj[[2]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_hj[[2]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_hj[[2]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_hj[[2]])

CC_2050_rcp26_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_hj[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_m_hj[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_u_hj[[3]],
                 df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_hj[[3]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_hj[[3]],
                 df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_hj[[3]])

CC_2050_rcp85_hj_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_hj[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_m_hj[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_u_hj[[3]],
                 df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_hj[[3]],
                 df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_hj[[3]],
                 df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_hj[[3]])

CCLC_2030_rcp26_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_hj[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_m_hj[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_u_hj[[1]],
                   df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_hj[[1]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_hj[[1]],
                   df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_hj[[1]])

CCLC_2030_rcp85_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_hj[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_m_hj[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_u_hj[[1]],
                   df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_hj[[1]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_hj[[1]],
                   df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_hj[[1]])

CCLC_2040_rcp26_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_hj[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_m_hj[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_u_hj[[2]],
                   df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_hj[[2]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_hj[[2]],
                   df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_hj[[2]])

CCLC_2040_rcp85_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_hj[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_m_hj[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_u_hj[[2]],
                   df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_hj[[2]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_hj[[2]],
                   df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_hj[[2]])

CCLC_2050_rcp26_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_hj[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_m_hj[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_u_hj[[3]],
                   df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_hj[[3]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_hj[[3]],
                   df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_hj[[3]])

CCLC_2050_rcp85_hj_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_hj[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_m_hj[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_u_hj[[3]],
                   df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_hj[[3]],
                   df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_hj[[3]],
                   df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_hj[[3]])

#site2
CC_2030_rcp26_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_hj[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_m_hj[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_u_hj[[1]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_hj[[1]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_hj[[1]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_hj[[1]])

CC_2030_rcp85_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_hj[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_m_hj[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_u_hj[[1]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_hj[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_hj[[1]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_hj[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_hj[[1]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_hj[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_hj[[1]])

CC_2040_rcp26_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_hj[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_m_hj[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_u_hj[[2]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_hj[[2]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_hj[[2]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_hj[[2]])

CC_2040_rcp85_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_hj[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_m_hj[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_u_hj[[2]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_hj[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_hj[[2]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_hj[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_hj[[2]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_hj[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_hj[[2]])

CC_2050_rcp26_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_hj[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_m_hj[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_u_hj[[3]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_hj[[3]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_hj[[3]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_hj[[3]])

CC_2050_rcp85_hj_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_hj[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_m_hj[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_u_hj[[3]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_hj[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_hj[[3]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_hj[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_hj[[3]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_hj[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_hj[[3]])

CCLC_2030_rcp26_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_hj[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_m_hj[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_u_hj[[1]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_hj[[1]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_hj[[1]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_hj[[1]])

CCLC_2030_rcp85_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_hj[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_m_hj[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_u_hj[[1]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_hj[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_hj[[1]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_hj[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_hj[[1]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_hj[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_hj[[1]])

CCLC_2040_rcp26_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_hj[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_m_hj[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_u_hj[[2]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_hj[[2]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_hj[[2]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_hj[[2]])

CCLC_2040_rcp85_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_hj[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_m_hj[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_u_hj[[2]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_hj[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_hj[[2]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_hj[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_hj[[2]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_hj[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_hj[[2]])

CCLC_2050_rcp26_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_hj[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_m_hj[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_u_hj[[3]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_hj[[3]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_hj[[3]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_hj[[3]])

CCLC_2050_rcp85_hj_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_hj[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_m_hj[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_u_hj[[3]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_hj[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_hj[[3]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_hj[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_hj[[3]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_hj[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_hj[[3]])



# harvest all
LC_2030_ha_s1<-rep(c(df_hazard_LU1[1,2],df_hazard_LU1[1,5],df_hazard_LU1[1,8]),time=4)
LC_2040_ha_s1<-rep(c(df_hazard_LU1[2,2],df_hazard_LU1[2,5],df_hazard_LU1[2,8]),time=4)
LC_2050_ha_s1<-rep(c(df_hazard_LU1[3,2],df_hazard_LU1[3,5],df_hazard_LU1[3,8]),time=4)

LC_2030_ha_s2<-rep(c(df_hazard_LU1[1,11],df_hazard_LU1[1,14],df_hazard_LU1[1,17]),time=4) # check numbers
LC_2040_ha_s2<-rep(c(df_hazard_LU1[2,11],df_hazard_LU1[2,14],df_hazard_LU1[2,17]),time=4)
LC_2050_ha_s2<-rep(c(df_hazard_LU1[3,11],df_hazard_LU1[3,14],df_hazard_LU1[3,17]),time=4)

H_2030_ha_s1<-rep(c(df_hazard_h1[1,1],df_hazard_h1[1,3],df_hazard_h1[1,5]),time=4)
H_2040_ha_s1<-rep(c(df_hazard_h1[2,1],df_hazard_h1[2,3],df_hazard_h1[2,5]),time=4)
H_2050_ha_s1<-rep(c(df_hazard_h1[3,1],df_hazard_h1[3,3],df_hazard_h1[3,5]),time=4)

H_2030_ha_s2<-rep(c(df_hazard_h1[1,7],df_hazard_h1[1,9],df_hazard_h1[1,11]),time=4) # check numbers
H_2040_ha_s2<-rep(c(df_hazard_h1[2,7],df_hazard_h1[2,9],df_hazard_h1[2,11]),time=4)
H_2050_ha_s2<-rep(c(df_hazard_h1[3,7],df_hazard_h1[3,9],df_hazard_h1[3,11]),time=4)

CC_2030_rcp26_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_ha[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_m_ha[[1]],df_hazard1$scen000_CanESM2_rcp26_s1_u_ha[[1]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_ha[[1]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_ha[[1]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_ha[[1]])

CC_2030_rcp85_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_ha[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_m_ha[[1]],df_hazard1$scen000_CanESM2_rcp85_s1_u_ha[[1]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_ha[[1]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_ha[[1]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_ha[[1]])

CC_2040_rcp26_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_ha[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_m_ha[[2]],df_hazard1$scen000_CanESM2_rcp26_s1_u_ha[[2]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_ha[[2]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_ha[[2]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_ha[[2]])

CC_2040_rcp85_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_ha[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_m_ha[[2]],df_hazard1$scen000_CanESM2_rcp85_s1_u_ha[[2]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_ha[[2]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_ha[[2]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_ha[[2]])

CC_2050_rcp26_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp26_s1_l_ha[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_m_ha[[3]],df_hazard1$scen000_CanESM2_rcp26_s1_u_ha[[3]],
                    df_hazard1$scen000_HadGEM2.ES_rcp26_s1_l_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_m_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s1_u_ha[[3]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_l_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_m_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s1_u_ha[[3]],
                    df_hazard1$scen000_MRI.CGCM3_rcp26_s1_l_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_m_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s1_u_ha[[3]])

CC_2050_rcp85_ha_s1<-c(df_hazard1$scen000_CanESM2_rcp85_s1_l_ha[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_m_ha[[3]],df_hazard1$scen000_CanESM2_rcp85_s1_u_ha[[3]],
                    df_hazard1$scen000_HadGEM2.ES_rcp85_s1_l_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_m_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s1_u_ha[[3]],
                    df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_l_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_m_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s1_u_ha[[3]],
                    df_hazard1$scen000_MRI.CGCM3_rcp85_s1_l_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_m_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s1_u_ha[[3]])

CCLC_2030_rcp26_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_ha[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_m_ha[[1]],df_hazard1$scen002_CanESM2_rcp26_s1_u_ha[[1]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_ha[[1]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_ha[[1]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_ha[[1]])

CCLC_2030_rcp85_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_ha[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_m_ha[[1]],df_hazard1$scen002_CanESM2_rcp85_s1_u_ha[[1]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_ha[[1]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_ha[[1]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_ha[[1]])

CCLC_2040_rcp26_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_ha[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_m_ha[[2]],df_hazard1$scen002_CanESM2_rcp26_s1_u_ha[[2]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_ha[[2]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_ha[[2]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_ha[[2]])

CCLC_2040_rcp85_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_ha[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_m_ha[[2]],df_hazard1$scen002_CanESM2_rcp85_s1_u_ha[[2]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_ha[[2]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_ha[[2]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_ha[[2]])

CCLC_2050_rcp26_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp26_s1_l_ha[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_m_ha[[3]],df_hazard1$scen002_CanESM2_rcp26_s1_u_ha[[3]],
                      df_hazard1$scen002_HadGEM2.ES_rcp26_s1_l_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_m_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s1_u_ha[[3]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_l_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_m_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s1_u_ha[[3]],
                      df_hazard1$scen002_MRI.CGCM3_rcp26_s1_l_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_m_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s1_u_ha[[3]])

CCLC_2050_rcp85_ha_s1<-c(df_hazard1$scen002_CanESM2_rcp85_s1_l_ha[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_m_ha[[3]],df_hazard1$scen002_CanESM2_rcp85_s1_u_ha[[3]],
                      df_hazard1$scen002_HadGEM2.ES_rcp85_s1_l_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_m_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s1_u_ha[[3]],
                      df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_l_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_m_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s1_u_ha[[3]],
                      df_hazard1$scen002_MRI.CGCM3_rcp85_s1_l_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_m_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s1_u_ha[[3]])

#site2

CC_2030_rcp26_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_ha[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_m_ha[[1]],df_hazard1$scen000_CanESM2_rcp26_s2_u_ha[[1]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_ha[[1]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_ha[[1]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_ha[[1]])

CC_2030_rcp85_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_ha[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_m_ha[[1]],df_hazard1$scen000_CanESM2_rcp85_s2_u_ha[[1]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_ha[[1]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_ha[[1]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_ha[[1]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_ha[[1]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_ha[[1]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_ha[[1]])

CC_2040_rcp26_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_ha[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_m_ha[[2]],df_hazard1$scen000_CanESM2_rcp26_s2_u_ha[[2]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_ha[[2]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_ha[[2]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_ha[[2]])

CC_2040_rcp85_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_ha[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_m_ha[[2]],df_hazard1$scen000_CanESM2_rcp85_s2_u_ha[[2]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_ha[[2]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_ha[[2]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_ha[[2]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_ha[[2]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_ha[[2]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_ha[[2]])

CC_2050_rcp26_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp26_s2_l_ha[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_m_ha[[3]],df_hazard1$scen000_CanESM2_rcp26_s2_u_ha[[3]],
                       df_hazard1$scen000_HadGEM2.ES_rcp26_s2_l_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_m_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp26_s2_u_ha[[3]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_l_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_m_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp26_s2_u_ha[[3]],
                       df_hazard1$scen000_MRI.CGCM3_rcp26_s2_l_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_m_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp26_s2_u_ha[[3]])

CC_2050_rcp85_ha_s2<-c(df_hazard1$scen000_CanESM2_rcp85_s2_l_ha[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_m_ha[[3]],df_hazard1$scen000_CanESM2_rcp85_s2_u_ha[[3]],
                       df_hazard1$scen000_HadGEM2.ES_rcp85_s2_l_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_m_ha[[3]],df_hazard1$scen000_HadGEM2.ES_rcp85_s2_u_ha[[3]],
                       df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_l_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_m_ha[[3]],df_hazard1$scen000_MPI.ESM.MR_rcp85_s2_u_ha[[3]],
                       df_hazard1$scen000_MRI.CGCM3_rcp85_s2_l_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_m_ha[[3]],df_hazard1$scen000_MRI.CGCM3_rcp85_s2_u_ha[[3]])

CCLC_2030_rcp26_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_ha[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_m_ha[[1]],df_hazard1$scen002_CanESM2_rcp26_s2_u_ha[[1]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_ha[[1]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_ha[[1]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_ha[[1]])

CCLC_2030_rcp85_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_ha[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_m_ha[[1]],df_hazard1$scen002_CanESM2_rcp85_s2_u_ha[[1]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_ha[[1]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_ha[[1]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_ha[[1]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_ha[[1]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_ha[[1]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_ha[[1]])

CCLC_2040_rcp26_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_ha[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_m_ha[[2]],df_hazard1$scen002_CanESM2_rcp26_s2_u_ha[[2]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_ha[[2]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_ha[[2]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_ha[[2]])

CCLC_2040_rcp85_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_ha[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_m_ha[[2]],df_hazard1$scen002_CanESM2_rcp85_s2_u_ha[[2]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_ha[[2]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_ha[[2]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_ha[[2]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_ha[[2]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_ha[[2]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_ha[[2]])

CCLC_2050_rcp26_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp26_s2_l_ha[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_m_ha[[3]],df_hazard1$scen002_CanESM2_rcp26_s2_u_ha[[3]],
                         df_hazard1$scen002_HadGEM2.ES_rcp26_s2_l_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_m_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp26_s2_u_ha[[3]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_l_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_m_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp26_s2_u_ha[[3]],
                         df_hazard1$scen002_MRI.CGCM3_rcp26_s2_l_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_m_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp26_s2_u_ha[[3]])

CCLC_2050_rcp85_ha_s2<-c(df_hazard1$scen002_CanESM2_rcp85_s2_l_ha[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_m_ha[[3]],df_hazard1$scen002_CanESM2_rcp85_s2_u_ha[[3]],
                         df_hazard1$scen002_HadGEM2.ES_rcp85_s2_l_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_m_ha[[3]],df_hazard1$scen002_HadGEM2.ES_rcp85_s2_u_ha[[3]],
                         df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_l_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_m_ha[[3]],df_hazard1$scen002_MPI.ESM.MR_rcp85_s2_u_ha[[3]],
                         df_hazard1$scen002_MRI.CGCM3_rcp85_s2_l_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_m_ha[[3]],df_hazard1$scen002_MRI.CGCM3_rcp85_s2_u_ha[[3]])


#########

data_2030_s1<-cbind(1,LC_2030_s1,CC_2030_rcp26_s1,CC_2030_rcp85_s1,CCLC_2030_rcp26_s1,CCLC_2030_rcp85_s1,
                H_2030_hj_s1, LC_2030_hj_s1,CC_2030_rcp26_hj_s1,CC_2030_rcp85_hj_s1,CCLC_2030_rcp26_hj_s1,CCLC_2030_rcp85_hj_s1,
                 H_2030_ha_s1,LC_2030_ha_s1,CC_2030_rcp26_ha_s1,CC_2030_rcp85_ha_s1,CCLC_2030_rcp26_ha_s1,CCLC_2030_rcp85_ha_s1)
  
data_2040_s1<-cbind(1,LC_2040_s1,CC_2040_rcp26_s1,CC_2040_rcp85_s1,CCLC_2040_rcp26_s1,CCLC_2040_rcp85_s1,
                 H_2040_hj_s1,LC_2040_hj_s1,CC_2040_rcp26_hj_s1,CC_2040_rcp85_hj_s1,CCLC_2040_rcp26_hj_s1,CCLC_2040_rcp85_hj_s1,
                 H_2040_ha_s1,LC_2040_ha_s1,CC_2040_rcp26_ha_s1,CC_2040_rcp85_ha_s1,CCLC_2040_rcp26_ha_s1,CCLC_2040_rcp85_ha_s1)

data_2050_s1<-cbind(1,LC_2050_s1,CC_2050_rcp26_s1,CC_2050_rcp85_s1,CCLC_2050_rcp26_s1,CCLC_2050_rcp85_s1,
                 H_2050_hj_s1,LC_2050_hj_s1,CC_2050_rcp26_hj_s1,CC_2050_rcp85_hj_s1,CCLC_2050_rcp26_hj_s1,CCLC_2050_rcp85_hj_s1,
                 H_2050_ha_s1,LC_2050_ha_s1,CC_2050_rcp26_ha_s1,CC_2050_rcp85_ha_s1,CCLC_2050_rcp26_ha_s1,CCLC_2050_rcp85_ha_s1)

data_2030_s2<-cbind(1,LC_2030_s2,CC_2030_rcp26_s2,CC_2030_rcp85_s2,CCLC_2030_rcp26_s2,CCLC_2030_rcp85_s2,
                    H_2030_hj_s2,LC_2030_hj_s2,CC_2030_rcp26_hj_s2,CC_2030_rcp85_hj_s2,CCLC_2030_rcp26_hj_s2,CCLC_2030_rcp85_hj_s2,
                    H_2030_ha_s2,LC_2030_ha_s2,CC_2030_rcp26_ha_s2,CC_2030_rcp85_ha_s2,CCLC_2030_rcp26_ha_s2,CCLC_2030_rcp85_ha_s2)

data_2040_s2<-cbind(1,LC_2040_s2,CC_2040_rcp26_s2,CC_2040_rcp85_s2,CCLC_2040_rcp26_s2,CCLC_2040_rcp85_s2,
                    H_2040_hj_s2,LC_2040_hj_s2,CC_2040_rcp26_hj_s2,CC_2040_rcp85_hj_s2,CCLC_2040_rcp26_hj_s2,CCLC_2040_rcp85_hj_s2,
                    H_2040_ha_s2,LC_2040_ha_s2,CC_2040_rcp26_ha_s2,CC_2040_rcp85_ha_s2,CCLC_2040_rcp26_ha_s2,CCLC_2040_rcp85_ha_s2)

data_2050_s2<-cbind(1,LC_2050_s2,CC_2050_rcp26_s2,CC_2050_rcp85_s2,CCLC_2050_rcp26_s2,CCLC_2050_rcp85_s2,
                    H_2050_hj_s2,LC_2050_hj_s2,CC_2050_rcp26_hj_s2,CC_2050_rcp85_hj_s2,CCLC_2050_rcp26_hj_s2,CCLC_2050_rcp85_hj_s2,
                    H_2050_ha_s2,LC_2050_ha_s2,CC_2050_rcp26_ha_s2,CC_2050_rcp85_ha_s2,CCLC_2050_rcp26_ha_s2,CCLC_2050_rcp85_ha_s2)


# boxplot
par(mfrow=c(2,3))
boxplot(data_2030_s1,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'), main='2030',ylim=c(0,1.1))
#()
boxplot(data_2040_s1,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'),main='2040',ylim=c(0,1.1))
#grid()
boxplot(data_2050_s1,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'),main='2050',ylim=c(0,1.1))
#grid()


boxplot(data_2030_s2,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'), main='2030',ylim=c(0,1.1))
#grid()
boxplot(data_2040_s2,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'),main='2040',ylim=c(0,1.1))
#grid()
boxplot(data_2050_s2,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'),main='2050',ylim=c(0,1.1))
#grid()

dev.off()

###############
data_2030_s1<-cbind(1,H_2030_hj_s1,H_2030_ha_s1,
                    LC_2030_s1,LC_2030_hj_s1, LC_2030_ha_s1,
                    CC_2030_rcp26_s1,CC_2030_rcp85_s1,CCLC_2030_rcp26_s1,CCLC_2030_rcp85_s1,
                    CC_2030_rcp26_hj_s1,CC_2030_rcp85_hj_s1,CC_2030_rcp26_ha_s1,CC_2030_rcp85_ha_s1,
                    CCLC_2030_rcp26_hj_s1,CCLC_2030_rcp85_hj_s1,CCLC_2030_rcp26_ha_s1,CCLC_2030_rcp85_ha_s1)

boxplot(data_2030_s1,col=c('grey','darkgoldenrod','lightgreen','darkgreen','lightblue','darkblue'), main='2030',ylim=c(0,1.1))



data_2040_s1<-cbind(1,LC_2040_s1,CC_2040_rcp26_s1,CC_2040_rcp85_s1,CCLC_2040_rcp26_s1,CCLC_2040_rcp85_s1,
                    H_2040_hj_s1,LC_2040_hj_s1,CC_2040_rcp26_hj_s1,CC_2040_rcp85_hj_s1,CCLC_2040_rcp26_hj_s1,CCLC_2040_rcp85_hj_s1,
                    H_2040_ha_s1,LC_2040_ha_s1,CC_2040_rcp26_ha_s1,CC_2040_rcp85_ha_s1,CCLC_2040_rcp26_ha_s1,CCLC_2040_rcp85_ha_s1)

data_2050_s1<-cbind(1,LC_2050_s1,CC_2050_rcp26_s1,CC_2050_rcp85_s1,CCLC_2050_rcp26_s1,CCLC_2050_rcp85_s1,
                    H_2050_hj_s1,LC_2050_hj_s1,CC_2050_rcp26_hj_s1,CC_2050_rcp85_hj_s1,CCLC_2050_rcp26_hj_s1,CCLC_2050_rcp85_hj_s1,
                    H_2050_ha_s1,LC_2050_ha_s1,CC_2050_rcp26_ha_s1,CC_2050_rcp85_ha_s1,CCLC_2050_rcp26_ha_s1,CCLC_2050_rcp85_ha_s1)

data_2030_s2<-cbind(1,LC_2030_s2,CC_2030_rcp26_s2,CC_2030_rcp85_s2,CCLC_2030_rcp26_s2,CCLC_2030_rcp85_s2,
                    H_2030_hj_s2,LC_2030_hj_s2,CC_2030_rcp26_hj_s2,CC_2030_rcp85_hj_s2,CCLC_2030_rcp26_hj_s2,CCLC_2030_rcp85_hj_s2,
                    H_2030_ha_s2,LC_2030_ha_s2,CC_2030_rcp26_ha_s2,CC_2030_rcp85_ha_s2,CCLC_2030_rcp26_ha_s2,CCLC_2030_rcp85_ha_s2)

data_2040_s2<-cbind(1,LC_2040_s2,CC_2040_rcp26_s2,CC_2040_rcp85_s2,CCLC_2040_rcp26_s2,CCLC_2040_rcp85_s2,
                    H_2040_hj_s2,LC_2040_hj_s2,CC_2040_rcp26_hj_s2,CC_2040_rcp85_hj_s2,CCLC_2040_rcp26_hj_s2,CCLC_2040_rcp85_hj_s2,
                    H_2040_ha_s2,LC_2040_ha_s2,CC_2040_rcp26_ha_s2,CC_2040_rcp85_ha_s2,CCLC_2040_rcp26_ha_s2,CCLC_2040_rcp85_ha_s2)

data_2050_s2<-cbind(1,LC_2050_s2,CC_2050_rcp26_s2,CC_2050_rcp85_s2,CCLC_2050_rcp26_s2,CCLC_2050_rcp85_s2,
                    H_2050_hj_s2,LC_2050_hj_s2,CC_2050_rcp26_hj_s2,CC_2050_rcp85_hj_s2,CCLC_2050_rcp26_hj_s2,CCLC_2050_rcp85_hj_s2,
                    H_2050_ha_s2,LC_2050_ha_s2,CC_2050_rcp26_ha_s2,CC_2050_rcp85_ha_s2,CCLC_2050_rcp26_ha_s2,CCLC_2050_rcp85_ha_s2)












### plot timeseries
c1 <- rainbow(10)
c2 <- rainbow(10, alpha=0.2)
c3 <- rainbow(10, v=0.7)

par(mfrow=c(2,6))

#site1
plot(time1,c(1,df_b[[1]]$mean),ylim=c(0,120),xlab='year',col='white', ylab='Abundance [%]')
polygon(c(time1, rev(time1)),c(c(100,((df_b[[1]]$mean-2*df_b[[1]]$sd)*100/32825)), rev(c(100,((df_b[[2]]$mean+2*df_b[[2]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)

plot(time1,c(1,df_h[[1]]$mean),ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df_h[[1]]$mean-2*df_h[[1]]$sd)*100/32825)), rev(c(100,((df_h[[3]]$mean+2*df_h[[3]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('LC', side=1, line=-14, at=2035,font=0.7)

plot(time1,c(1,df_l[[1]]$mean),ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df_l[[1]]$mean-2*df_l[[1]]$sd)*100/32825)), rev(c(100,((df_l[[4]]$mean+2*df_l[[4]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('LC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[1]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[1]]$mean-2*df[[1]]$sd)*100/32825)), rev(c(100,((df[[4]]$mean+2*df[[4]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[37]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[73]]$mean-2*df[[73]]$sd)*100/32825)), rev(c(100,((df[[76]]$mean+2*df[[76]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CCLC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[37]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[74]]$mean-2*df[[74]]$sd)*100/32825)), rev(c(100,((df[[77]]$mean+2*df[[77]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CCLC', side=1, line=-14, at=2035,font=0.7)


#site2
plot(time1,c(1,df_b[[1]]$mean),ylim=c(0,380),xlab='year',col='white', ylab='Abundance [%]')
polygon(c(time1, rev(time1)),c(c(100,((df_b[[4]]$mean-2*df_b[[4]]$sd)*100/32825)), rev(c(100,((df_b[[5]]$mean+2*df_b[[5]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('Baseline', side=1, line=-14, at=2035,font=0.7)

plot(time1,c(1,df_h[[1]]$mean),ylim=c(0,380),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df_h[[7]]$mean-2*df_h[[7]]$sd)*100/32825)), rev(c(100,((df_h[[9]]$mean+2*df_h[[9]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('LC', side=1, line=-14, at=2035,font=0.7)

plot(time1,c(1,df_l[[1]]$mean),ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df_l[[10]]$mean-2*df_l[[10]]$sd)*100/32825)), rev(c(100,((df_l[[13]]$mean+2*df_l[[13]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('LC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[1]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[10]]$mean-2*df[[10]]$sd)*100/32825)), rev(c(100,((df[[13]]$mean+2*df[[13]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[37]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[82]]$mean-2*df[[82]]$sd)*100/32825)), rev(c(100,((df[[85]]$mean+2*df[[85]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CCLC', side=1, line=-14, at=2035,font=0.7)

plot(time,df[[37]]$mean,ylim=c(0,120),xlab='year',col='white', ylab=' ')
polygon(c(time1, rev(time1)),c(c(100,((df[[83]]$mean-2*df[[83]]$sd)*100/32825)), rev(c(100,((df[[86]]$mean+2*df[[86]]$sd)*100/32825)))), col='gray70', border =NA)
abline(a=100,b=0)#mtext('CanESM2 RCP2.6 CCLC', side=1, line=-14, at=2035,font=0.7)

dev.off()
##### area


# input data
infile_midyear<-read.csv('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/first_round/analysis/Clivia_miniata_area.csv')
data_mid<-as.data.frame(infile_midyear)
modelnames_mid<-names(data_mid)

area_CC_min26<-rowMins(cbind(data_mid[[2]],data_mid[[6]],data_mid[[10]],data_mid[[14]]))
area_CC_max26<-rowMaxs(cbind(data_mid[[2]],data_mid[[6]],data_mid[[10]],data_mid[[14]]))
area_CCLC_min26<-rowMins(cbind(data_mid[[3]],data_mid[[7]],data_mid[[11]],data_mid[[15]]))
area_CCLC_max26<-rowMaxs(cbind(data_mid[[3]],data_mid[[7]],data_mid[[11]],data_mid[[15]]))
area_LC<-data_mid[[18]]

area_Canesm26_CC<-data_mid[[6]]
area_Canesm26_CCLC<-data_mid[[7]]

par(mfrow=c(2,5))

# empty
plot(time,area_LC*100/area_LC[[1]],xlab='year',col='white', ylim=c(0,150),ylab='Change in area [%]')
abline(h=100)
# area LC

plot(time,area_LC*100/area_LC[[1]],xlab='year',col='white', ylim=c(0,150), ylab='Change in area [%]')
lines(time1, c(100,area_LC*100/area_LC[[1]]),col='gray70',lwd=2)

# area CC
plot(time,area_CC_min26*100/area_CC_min26[[1]],xlab='year',col='white', ylim=c(0,150), ylab='Change in area [%]')
lines(time1, c(100,area_Canesm26_CC[1:30]*100/area_Canesm26_CC[[1]],area_Canesm26_CC[[30]]*100/area_Canesm26_CC[[1]]),col='gray70',lwd=2)

#area CCLC
plot(time,area_CCLC_min26*100/area_CCLC_min26[[1]],xlab='year',col='white', ylim=c(0,150), ylab='Change in area [%]')
lines(time1, c(100,area_Canesm26_CCLC[1:30]*100/area_Canesm26_CCLC[[1]],area_Canesm26_CCLC[[30]]*100/area_Canesm26_CCLC[[1]]),col='gray70',lwd=2)

# area CCLC harvest
plot(time,area_CCLC_min26*100/area_CCLC_min26[[1]],xlab='year',col='white', ylim=c(0,150), ylab='Change in area [%]')
lines(time1, c(100,area_Canesm26_CCLC[1:30]*100/area_Canesm26_CCLC[[1]],area_Canesm26_CCLC[[30]]*100/area_Canesm26_CCLC[[1]]),col='gray70',lwd=2)

#abundance baseline
plot(time1,c(1,df_b[[1]]$mean),ylim=c(0,150),xlab='year',col='white', ylab='Change in abundance [%]')
polygon(c(time1, rev(time1)),c(c(100,(df_b[[1]]$min*100/32825)), rev(c(100,(df_b[[2]]$max*100/32825)))), col='gray70', border =NA)

# abundance LC
plot(time1,c(1,df_l[[1]]$mean),ylim=c(0,150),xlab='year',col='white', ylab='Relative abundance')
polygon(c(time1, rev(time1)),c(c(100,(df_l[[1]]$min*100/32825)), rev(c(100,(df_l[[4]]$max*100/32825)))), col='gray70', border =NA)

# abundance CanESM2 RCP2.6 CC
plot(time,df[[1]]$mean,ylim=c(0,150),xlab='year',col='white', ylab='Hazard rate')
polygon(c(time1, rev(time1)),c(c(100,(df[[1]]$min*100/32825)), rev(c(100,(df[[4]]$max*100/32825)))), col='gray70', border =NA)

# abundance CanESM2 RCP2.6 CCLC
plot(time,df[[37]]$mean,ylim=c(0,150),xlab='year',col='white', ylab='Hazard rate')
polygon(c(time1, rev(time1)),c(c(100,(df[[37]]$min*100/32825)), rev(c(100,(df[[40]]$max*100/32825)))), col='gray70', border =NA)

# abundance CanESM2 RCP2.6 CCLC ALL
plot(time,df[[37]]$mean,ylim=c(0,150),xlab='year',col='white', ylab='Hazard rate')
polygon(c(time1, rev(time1)),c(c(100,(df[[38]]$min*100/32825)), rev(c(100,(df[[41]]$max*100/32825)))), col='gray70', border =NA)


