#  calculate species Land cover preference mask

# data source
path1<-"Y:/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/"

# load predictor MODIS file
f <- raster(paste(path1,'MODIS_LCType.grd',sep=''))

new_LCType<-f
# this selection is still manually
new_LCType[new_LCType==0]<-0 # water
new_LCType[new_LCType==1]<-1 # Evergreen Needleleaf forest
new_LCType[new_LCType==2]<-1 # Evergreen Broadleaf forest
new_LCType[new_LCType==3]<-1 # Deciduous Needleleaf forest
new_LCType[new_LCType==4]<-1 # Deciduous Broadleaf forest
new_LCType[new_LCType==5]<-1 # Mixed forest
new_LCType[new_LCType==6]<-1 # closed shrubland
new_LCType[new_LCType==7]<-1 # open shrubland
new_LCType[new_LCType==8]<-1 # woody savanna
new_LCType[new_LCType==9]<-1 # savanna
new_LCType[new_LCType==10]<-0 # grassland
new_LCType[new_LCType==11]<-0 # permanent wetlands
new_LCType[new_LCType==12]<-0 # croplands
new_LCType[new_LCType==13]<-0 # urban and built up
new_LCType[new_LCType==14]<-1 # Cropland/Natural vegetation mosaic
new_LCType[new_LCType==15]<-0 # snow and ice
new_LCType[new_LCType==16]<-0 # Barren or sparsely vegetated
new_LCType

plot(new_LCType)
writeRaster(new_LCType, file=paste(path11,'Clivia_miniata_LU_mask.grd',sep=''),overwrite=TRUE)
