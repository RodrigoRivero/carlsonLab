import pyhdf.SD
from pprint import pprint
from pyhdf.SD import SD, SDC
import numpy
import os.path
import glob

### opening the file

# Directory to scan for HDF files and save output TXT
root = "/Users/Char/googleDrive/tomoProject/MODIS_h08v05/"

# Different resolutions seem to need different pixel start positions
# Here each start position can be defined by the resolution of the file
# The start position is the top left corner, in the format (x, y)
startIndicesByResolution = {
    "500m" : (358, 2266),
    "1km" : (179, 1133)
}

# Same as above, but for how wide/long the selected rectangle should be
countsByResolution = {
    "500m" : (4, 6),
    "1km" : (2, 3)
}

# Not all HDF files have a resolution, assume this resolution if we can't find one
defaultResolutionIfNoneFound = "1km"

# Names of the bands to pull out
bands = ["state_1km_1",
        "SensorZenith_1",
        "SensorAzimuth_1",
        "SolarZenith_1",
        "SolarAzimuth_1",
        "sur_refl_b01_1",
        "sur_refl_b02_1",
        "sur_refl_b03_1",
        "sur_refl_b04_1",
        "sur_refl_b05_1",
        "sur_refl_b06_1",
        "sur_refl_b07_1",]

# Find all HDF files in the directory defined above
# Find all HDF files including inside sub-directories hdfFiles = glob.glob(os.path.join(root, "**/*.hdf"))
hdfFiles = glob.glob(os.path.join(root, "*.hdf"))

# Takes the data and writes it to the file
def writeToFile(fileName, pixels, ndvi, evi, evi2):
    print("Writing to " + fileName)
    hdfName = os.path.splitext(os.path.split(fileName)[1])[0]
    with open(fileName, "w") as file_out:
        file_out.write(hdfName + "\n")
        for bandName in bands:
            file_out.write(bandName + "\n")
            file_out.write(str(pixels[bandName]) + "\n")
        file_out.write("NDVI\n")
        file_out.write(str(ndvi) + "\n")
        file_out.write("EVI\n")
        file_out.write(str(evi) + "\n")
        file_out.write("EVI2\n")
        file_out.write(str(evi2) + "\n")

# Loop over every HDF file found
for hdfFile in hdfFiles:
    #Reads in HDF file
    hdfSD = SD(hdfFile, SDC.READ)
    print(hdfSD.datasets())
    #Selects only the bands specified above
    selectedBands = [hdfSD.select(band) for band in bands]
    selectedPixels = {}
    # Select only the desired range of pixels for each band
    for band in selectedBands:
        bandName = band.info()[0]
        # Find the resolution, or default resolution
        resolution = band.attributes()["Nadir Data Resolution"] if "Nadir Data Resolution" in band.attributes() else defaultResolutionIfNoneFound
        startIndices = startIndicesByResolution[resolution]
        counts = countsByResolution[resolution]
        pixels = band.get(start=startIndices, count=counts)
        selectedPixels[bandName] = pixels
        print(bandName)
        print(pixels)
    
    # Calculate NDVI
    ndvi_top = (selectedPixels["sur_refl_b02_1"] - selectedPixels["sur_refl_b01_1"]).astype(float)
    ndvi_bottom = (selectedPixels["sur_refl_b02_1"] + selectedPixels["sur_refl_b01_1"]).astype(float)
    ndvi =  ndvi_top / ndvi_bottom 
    print("ndvi", ndvi)

    # Calculate EVI
    evi_top = 2.5 * (selectedPixels["sur_refl_b02_1"] - selectedPixels["sur_refl_b01_1"])
    evi_bottom = selectedPixels["sur_refl_b02_1"] + (6 * selectedPixels["sur_refl_b01_1"]) + (7.5 * selectedPixels["sur_refl_b03_1"]) + 1 * 10000.0
    evi = (evi_top / evi_bottom) 
    print("evi", evi)

    # Calculate EVI2
    evi2_top = 2.5 * (selectedPixels["sur_refl_b02_1"] - selectedPixels["sur_refl_b01_1"])
    evi2_bottom = selectedPixels["sur_refl_b02_1"] + (2.4 * selectedPixels["sur_refl_b01_1"]) + 1 * 10000.0
    evi2 = (evi2_top / evi2_bottom) 
    print("evi2", evi2)

    # Output file name is the same as the HDF file, but with the extension '.txt'
    outputTextFile = os.path.splitext(hdfFile)[0] + ".txt"
    writeToFile(outputTextFile, selectedPixels, ndvi, evi, evi2)