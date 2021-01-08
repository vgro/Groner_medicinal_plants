#################################################################
## calculate change in area % for scenarios
##################################################################

inimap<-raster('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/scen000/MODIS_LCType.grd')
endmap<-raster('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/scen002/MODIS_LCType_scen002_year_2045.grd')
HS_map<-raster("C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/output/scen000/output_SDM/Clivia_miniata_HSmap.asc")
HS_map1<-HS_map>0.264
species_mask<-raster('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/Clivia_all_LU_mask.grd')
species_mask[species_mask==0]<-NA
landcells<-cellStats(!is.na(HS_map1),stat='sum')

inimap_c<-crop(inimap,HS_map1)
endmap_c<-crop(endmap,HS_map1)
inimap_c[inimap_c>0]<-1
endmap_c[endmap_c==0]<-NA
inimap_sum<-cellStats(!is.na(inimap_c*species_mask),stat='sum')
inimap_sum
inimap_sum_rel<-inimap_sum*100/landcells
inimap_sum_rel

endmap_sum<-cellStats(!is.na(endmap_c*species_mask),stat='sum')
endmap_sum
endmap_sum_rel<-endmap_sum*100/landcells
endmap_sum_rel


#############################################################################################
#### END ###     
#############################################################################################
