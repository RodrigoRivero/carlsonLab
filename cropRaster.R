library(raster)
library(rgdal)
library(maptools)
library(sp)
newtif <- raster(" ~/googleDrive/tomoProject/MODIS_jan4/test_Band12h08v05_2400WGS5.tif")




plot(newtif,
      main="modis")

hist(newtif)
setwd("~/googleDrive/tomoProject")

require(rgdal)
#read shapefile.shp from the current or working directory, readOGR("path to directory that holds the file", "shapefile name without the .shp")
studySite <- readOGR(".","radCalNet")



plot(newtif,
     main="modis-not cropped")

# crop the newtif by the studysite shapefile polygon
newtif_crop <- crop(x = newtif, y = studySite)

#plot the full modis tile boundary as an outline
plot(extent(newtif),
  lwd=4,col="springgreen",
  main="modis tile extent",
  xlab="easting", ylab="northing")

#plot the cropped section of newtif
plot(newtif_crop)

newtif_crop
#-----------------------------------------------------------
#plot the cropped section of newtif
plot(newtif_crop)

#see a histogram of the data in newtif_crop
hist(newtif_crop)

histinfo<-hist(newtif_crop)
histinfo
#----------------------------------------------------------

