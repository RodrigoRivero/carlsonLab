library(rgdal)
library(gdalUtils)
library(raster)
library(maptools)
library(sp)
setwd("~/googleDrive/tomoproject/MODIS_jan4")


out.files <- list.files(getwd(), pattern="hdf$", full.names=FALSE) #create a list with names of the .hdf files (they should be stored in your workspace)


#extracts the NDVI band (sd_index=1) and converts to .tiff
gdal_translate("MOD09GA.A2017172.h08v05.006.2017174031740.hdf","test_Band2h08v05.tif",sd_index=2)


#handles the conversion to WGS84
gdalwarp("test_Band2h08v05.tif","test_Band2h08v05_WGS.tif",s_srs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs",-ts 2400 2400, t_srs="+proj=longlat +datum=WGS84 +no_defs",srcnodata=-3000,dstnodata=-3000)




#howMany <- 102
#i <- 1
#setwd("new folder path")

#while (i <= howMany) {
#  out.files[i]
#  gdal_translate(out.files[i],newNames[i],sd_index=12)
#  i <- i + 1
#}