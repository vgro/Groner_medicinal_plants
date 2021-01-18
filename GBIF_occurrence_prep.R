#-----------------------------------------------------------------------------
# script to download and prepare species occurence
#-----------------------------------------------------------------------------

# load libraries
library(dismo)
library(rgdal)  
library(raster) 
library(sp)
library(maptools)

setwd('path_to_data')
  #load species occurrence
  species_genus <- 'Clivia'
  species_species<-'miniata'
  species_gbif <- gbif(paste(species_genus,species_species), geo=TRUE)
  species_gbif
  
  # lat lon available  
  species_geo <- subset(species_gbif, !is.na(lon) & !is.na(lat))
  species_geo
  # lat lon crop
  species_africa <- subset(species_geo, lon > -20 & lon < 50 & lat > -40 & lat < 0)
  species_africa
  # remove duplicates
  dups <- duplicated(species_africa[, c('lon', 'lat')])
  species_single <- species_africa[!dups, ]
  species_single
  # after 1960
  date <- species_single$year
  species_recent <- subset(species_single, date > 1990)
  print(paste('occurrence records for', species_genus,  nrow(species_recent),sep=' '))
    
  tiff(paste(species_genus, species_species,'_records.tiff',sep=''))
  data(wrld_simpl) # load world map
  plot(wrld_simpl, xlim=c(10,40), ylim=c(-40,0), axes=TRUE, col="light yellow", main=paste(species_genus,species_species,sep=' '))
  box()   
  coordinates(species_recent) <- ~lon+lat
  crs(species_recent) <- "+proj=longlat +datum=WGS84 +no_defs"
  points(species_recent, col='green',pch=19)
  dev.off()
      
  write.csv(species_recent, file=paste("occurrence_prep_", species_genus,"_",species_species,"_new.csv", sep=""))












