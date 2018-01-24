library(rgdal)
library(gdalUtils)
library(raster)
library(maptools)
library(sp)
library(rhdf5)
library(rasterVis)

gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)

#Set the input directory where the HDF Images are located
in_dir <- ("/Users/Char/googleDrive/tomoproject/MODIS_h08v05")

#This part pulls the names of the files in the directory that are HDF files
list.files(in_dir,pattern =".hdf$")


# Set output directory
output_folder <- ("/Users/Char/googleDrive/tomoProject/MODIS_h08v05_outDir")

#extracts the desired band (sd_index=2,12,13) and converts to .tiff. The outsuffix adds the name of the band to the end of the original name
batch_gdal_translate(infiles = in_dir, outdir = output_folder, outsuffix = "_band13.tif", pattern = ".hdf$", sd_index = 13)

#, of = "GTiff"
#This lists all the new TIFs in the out directory.
list.files(output_folder,pattern =".tif$")

##------------------------------------------------------------------------------
###Making a Raster stack of all of the Geotiffs 
library(rgdal)
library(raster)

# assign path to object = cleaner code
MODIS_outDirPath <- ("/Users/Char/googleDrive/tomoProject/MODIS_h08v05_outDir")

#Create a list of all MODIS H08V05 File paths for a band type
ModisH08V05_band3 <- list.files(MODIS_outDirPath, full.names=TRUE, pattern = ".tif$")

#view the list
ModisH08V05_band3

# Create a time series raster stack
ModisStack <- stack(ModisH08V05_band3)

#View the cooridnate reference system of the raster stack
crs(ModisStack)


extent(ModisStack)

# view a plot of all of the rasters
# 'nc' specifies number of columns (we will have 13 plots)
plot(ModisStack) 
#zlim = c(1500, 10000), 
#nc = 4)
###-----------------------------------------------------------
###CROPPING THE RASTER STACK

# Shapefile Path
studySitePath <- ("/Users/Char/googleDrive/tomoProject/")

#read shapefile.shp from the current or working directory, readOGR("path to directory that holds the file", "shapefile name without the .shp") it is studySite <- (".",shapefileName) if the shapefile is in the wd

studySite <- readOGR(studySitePath,"RadCalNetSinusoidal")

# crop the raster stack by the studysite shapefile polygon
Modis_crop <- crop(x = ModisStack, y = studySite)

#View histograms of the MODIS rasters
hist(Modis_crop)

# view a plot of all of the rasters
# 'nc' specifies number of columns (we will have 13 plots)
plot(Modis_crop, 
     zlim = c(1500, 10000), 
     nc = 4)



###------------------------------------------------------------------
##Exporting the raster stack data to CSV

#Make a matrix of the raster stack
modisMatrix <- getValues(Modis_crop)

modisMatrix

##Transpose the Axis to make the table easier to read
transposedMatrix <- t(modisMatrix)


#export the matrix to CSV 
write.csv(transposedMatrix, file="/Users/Char/googleDrive/tomoProject/modisMatrix3.csv")
##--------------------------------------------------------------------------------




