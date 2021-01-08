##################################################################
# script to prepare SDM output for RAMAS
# Land cover change only scenario
##################################################################

library(rgdal)  
library(raster) 
library(sp)
library(readr)
library(SpaDES)

# data source
species_name <-'Clivia_miniata'
years<-2015:2055
LU_scen<-"scen002"

# RAMAS specific
threshold<-0.264
nd<-5.0
ini_pop<-'500*log(noc)*ahs' 
capacity<-'500*noc*ahs' 
extent<-960

# directories
indir<- paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/',LU_scen,'/',sep='')
setwd(indir)

outdir<- paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/',LU_scen,'/RAMAS_spatial_LUonly/', sep='')
outdir_split<- paste(outdir,'HS_maps/',sep='')

# dummy file for .ptc
mystring <- read_file(paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/spatial_dummy_home_LUonly.txt',sep=''))
mystring

# load HS maps for species
Clivia_HS_maps<-raster('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/output/scen000/output_SDM/Clivia_miniata_HSmap.asc')
Clivia_HS_maps

HS_maps_in1 <- list.files(indir,pattern='grd')
HS_maps_in<-paste(indir, HS_maps_in1, sep="")
HS_maps_in

vp_r<-lapply(HS_maps_in,raster)

vp6<-list()
 for (m in 1:length(vp_r)){
   vp5<-vp_r[[m]]*Clivia_HS_maps
   vp6[[m]]<-resample(vp5,ensemble_raster)  
   writeRaster(vp6[[m]], paste(outdir_split,species_name,'_',LU_scen,"_LUonly_HS",years[[m]],'_HS.asc', sep = ""), overwrite = TRUE)
 }
plot(vp6[[15]])


# write .ptc
for (l in 1:length(years)){
 print(years[[l]])

  hs_map<-paste(species_name,'_',LU_scen,"_LUonly_HS",years[[l]], sep = "")
  y1<-gsub("species_name",species_name, mystring )
  y2<-gsub("threshold",threshold, y1)
  y3<-gsub("ND1", nd, y2)
  y4<-gsub("ini_pop", ini_pop, y3)
  y5<-gsub("LU_scen", LU_scen[[x]], y4)
  y6 <- gsub("hs_map",hs_map, y5)
  y8<-gsub('extent',extent,y6)
  y9<-gsub('additional_info','',y8)
  y10 <- gsub("capacity",capacity, y9)
  data_m[[i]]<-y10
  i=i+1
  write(y,paste(outdir,species_name,'_',LU_scen,'_LUonly_',years[[l]],".ptc",sep=''))
  
}


#############################################################################################
#### END ###     
#############################################################################################
