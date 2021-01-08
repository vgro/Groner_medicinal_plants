##################################################################
# script to prepare SDM output for RAMAS
# - average over 10 years
##################################################################

library(rgdal)  
library(raster) 
library(sp)
library(readr)
library(SpaDES)

# data source
path<-'C:/Users/Vivienne Groner/Desktop/Vivienne/2020/'
models<-c('CanESM2','HadGEM2-ES',"MPI-ESM-MR",'MRI-CGCM3')
years<-2015:2054
scenarios<-c("rcp26","rcp85")
LU_scen<-c('scen000','scen002')
species_name<-'Clivia_miniata'
i=1

for (x in 1:length(LU_scen)){
  path1<-paste(path,'SDM/',species_name,'_2020/output/',LU_scen[[x]],'/output_projected/',sep='')
  path2<-paste(path,'RAMAS/',species_name,'_2020/',LU_scen[[x]],'/RAMAS_spatial/HS_maps/',sep='')
  setwd(path1)

# load HS maps for species
  HS_maps_in1 <- list.files(path1,pattern='asc')
  HS_maps_in<-paste(path1, HS_maps_in1, sep="")
  
  data_m<-list()
  for (k in 1:length(models)) {
    for (m in 1:length(scenarios)){
      print(paste(models[[k]], scenarios[[m]],LU_scen[[x]], sep=' '))
      vp1<-HS_maps_in[grep(pattern=models[[k]],HS_maps_in)]
      vp2<-vp1[grep(pattern=scenarios[[m]],vp1)]
      vp3<-vp2[1:length(years)]
      vp4<-lapply(vp3,raster)
      
      for (l in 1:31){
        print(paste('year',years[[l+5]],  sep=' '))
        start<-l
        ende<-9+l
        mid<-5+l
        # calculate mean
        data<-vp4[start:ende]
        data_s<-stack(data)
        data_m[[i]]<-mean(data_s)
        crs(data_m[[i]]) <- "+proj=longlat +datum=WGS84 +no_defs"
        names(data_m[[i]])<-paste(species_name,'_',models[[k]],'_',scenarios[[m]],'_',LU_scen[[x]],'_',years[[mid]],sep='')
        writeRaster(data_m[[i]], paste(path2,species_name,"_",models[[k]],'_',scenarios[[m]],'_',LU_scen[[x]],"_",years[[mid]],'_HS.asc', sep = ""), overwrite = TRUE)
        i=i+1
      }
    }
  }
}


#############################################################################################
#### END ###     
#############################################################################################
      
