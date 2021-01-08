#############################################################################
# modify land cover to change 1% per year over 30 years, time step 5 years
#############################################################################

library(raster)
library(SpaDES)
memory.limit(size=500000000000)

# data source
years<-c(2020,2025,2030,2035,2040,2045)
r<-raster('Y:/Vivienne/2020/SDM/Clivia_all_2020/predictors/MODIS/Clivia_all_LU_mask.grd')
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(r)

e<-c(25,33,-34,-24)
rc<-crop(r,e)
plot(rc)

r_split<-splitRaster(rc,4,4, path='Y:/Vivienne/2020/SDM/Clivia_all_2020/predictors/MODIS/scen002/')
rook <- matrix(c(NA, 1, NA, 
                 1, 0,  1, 
                 NA, 1, NA), ncol=3, byrow=TRUE)

figures1<-list()

for (i in 1:length(r_split))  {

  print(paste('tile ',i,sep=''))
  rc<-r_split[[i]]
  xy<-as.data.frame(rc,xy=TRUE)                            # raster as data.frame
  colnames(xy)<-c('x','y','MODIS_LCType')                  # exctract values from raster
  xy1 <- data.frame(xyFromCell(rc, 1:ncell(rc)),xy=TRUE)   # extract lat lon from raster
  start_cells1<-subset(xy,xy$MODIS_LCType==0)             # subset cells that start with 0
  start_cells<-cellFromXY(rc,start_cells1)
  new_start_cells<-list()
  new_start_cells[[1]]<-list(start_cells)
  figures<-list()
  rc1<-rc

  for (y in 1:length(years)){
    print(paste('year',years[[y]], sep=' '))
    ra<-adjacent(rc, cells=unlist(new_start_cells[[y]]), directions=rook)        # find cells adjacent to zeros (start_cells)
    new_cells<-xy[ra[,'to'],]
    new_cells$MODIS_LCType<-0
    new_cell_num<-as.numeric(rownames(new_cells))
    rc1[new_cell_num]<-0
    new_start_cells[[y+1]]<-unique(new_cell_num)
    print('new cells selected')
    figures[[y]]<-rc1
    new_start_cells[[y]]<-0
  }
  figures1[[i]]<-figures
}


figures2<-unlist(figures1)
merged<-list()
for (i in 1:length(years)){
  len<-length(years)
  len1<-c(i,i+len,i+2*len,i+3*len,i+4*len,i+5*len,i+6*len,i+7*len,i+8*len,i+9*len,i+10*len,i+11*len,i+12*len,i+13*len,i+14*len,i+15*len)
  
  figures4<-list(c(figures2[[len1[[1]]]],figures2[[len1[[2]]]],figures2[[len1[[3]]]],figures2[[len1[[4]]]],
                   figures2[[len1[[5]]]],figures2[[len1[[6]]]],figures2[[len1[[7]]]],figures2[[len1[[8]]]],
                   figures2[[len1[[9]]]],figures2[[len1[[10]]]],figures2[[len1[[11]]]],figures2[[len1[[12]]]],
                   figures2[[len1[[13]]]],figures2[[len1[[14]]]],figures2[[len1[[15]]]],figures2[[len1[[16]]]]))
  
  figures5<-unlist(figures4)
  merged[[i]]<-mergeRaster(figures5)
  writeRaster(merged[[i]],paste('Y:/Vivienne/2020/SDM/Clivia_all_2020/predictors/MODIS/scen002/MODIS_LCType_scen002_year_',years[[y]],'.grd',sep=''),overwrite=TRUE)
}

merged_all<-stack(merged)
plot(merged_all)

for ( k in 1:6){
  writeRaster(merged[[k]],paste('Y:/Vivienne/2020/SDM/Clivia_all_2020/predictors/MODIS/scen002/MODIS_LCType_scen002_year_',years[[k]],'.grd',sep=''),overwrite=TRUE)
}

# adjust manually to write to other years inbetween transitions !
years1<-c(2024,2029,2034,2039,2044,2049)
for ( k in 1:6){
  writeRaster(merged[[k]],paste('Y:/Vivienne/2020/SDM/Clivia_all_2020/predictors/MODIS/scen002/MODIS_LCType_scen002_year_',years1[[k]],'.grd',sep=''),overwrite=TRUE)
}


#############################################################################################
#### END ###     
#############################################################################################

