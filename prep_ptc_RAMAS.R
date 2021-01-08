##################################################################
# script to  write .ptc files
##################################################################

library(rgdal)  
library(raster) 
library(sp)
library(readr)
library(SpaDES)

# inputs
species_name <-'Clivia_miniata'
models<-c('CanESM2','HadGEM2-ES','MPI-ESM-MR','MRI-CGCM3')
years<-2020:2050#c(2020,2025,2030,2035,2040,2045,2050)
scenarios<-c('rcp26','rcp85')
LU_scen<-c('scen000','scen002')

# RAMAS parameters
threshold<-0.264
nd<-5.0
ini_pop<-'500*log(noc)*ahs' 
capacity<-'500*noc*ahs' 
extent<-960

# dummy file for .ptc
mystring <- read_file(paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/spatial_dummy.txt',sep=''))
mystring

for (x in 1:length(LU_scen)){
  # directories
  indir<- paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/',LU_scen[[x]],'/RAMAS_spatial/HS_maps/',sep='')
  outdir<- paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/RAMAS/Clivia_miniata_2020/',LU_scen[[x]],'/RAMAS_spatial/',sep='')
  setwd(indir)
  
  # load HS maps for species
  HS_maps_in1 <- list.files(indir,pattern='asc')
  HS_maps_in<-paste(indir, HS_maps_in1, sep="")
  HS_maps_in
  i=1
  
  data_m<-list()
  for (j in 1:length(models)){
    for ( k in 1:length(scenarios)){
      vp1<-HS_maps_in[grep(pattern=models[[j]],HS_maps_in)]
      vp2<-vp1[grep(pattern=scenarios[[k]],vp1)]
      vp3<-vp2[1:length(years)]
      vp4<-lapply(HS_maps_in,raster)
      
      for (l in 1:length(years)){
        print(paste('year', years[[l]],sep=' '))
        
        # write .ptc
        hs_map<-paste(species_name,'_', models[[j]],"_" ,scenarios[[k]],"_",LU_scen[[x]],'_',years[[l]],'_HS', sep = "")
        y1<-gsub("species_name",species_name, mystring )
        y2<-gsub("threshold",threshold, y1)
        y3<-gsub("ND1", nd, y2)
        y4<-gsub("ini_pop", ini_pop, y3)
        y5<-gsub("LU_scen", LU_scen[[x]], y4)
        y6 <- gsub("hs_map",hs_map, y5)
        y8<-gsub('extent',extent,y6)
        y9<-gsub('additional_info','',y8)
        y10 <- gsub("capacity",capacity, y9)
        y11 <- gsub("models",models[[j]], y10)
        y <- gsub("scenarios",scenarios[[k]], y11)
        data_m[[i]]<-y
        i=i+1
        write(y,paste(outdir,species_name,'_',LU_scen[[x]],'_',models[[j]],'_',scenarios[[k]],'_',years[[l]],".ptc",sep=''))
        
      }
    }
  }
}


#############################################################################################
#### END ###     
#############################################################################################


             