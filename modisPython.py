import pyhdf.SD
from pprint import pprint
from pyhdf.SD import SD, SDC
import numpy
import os.path
import glob

### opening the file
# modisHDF = ("/Users/Char/googleDrive/tomoProject/MODIS_h08v05/MOD09GA.A2017173.h08v05.006.2017175031514.hdf")
root = "/Users/Char/googleDrive/tomoProject/MODIS_h08v05/"

startIndicesByResolution = {
    "500m" : (1134*2, 180*2),
    "1km" : (1134, 180)
}

countsByResolution = {
    "500m" : (2*2, 3*2),
    "1km" : (2, 3)
}

defaultResolutionIfNoneFound = "1km"

bands = ["state_1km_1",
        "SensorZenith_1",
        "SensorAzimuth_1",
        "SolarZenith_1",
        "SolarAzimuth_1",
        "sur_refl_b01_1",
        "sur_refl_b02_1",
        "sur_refl_b03_1",
        "sur_refl_b04_1"]

hdfFiles = glob.glob(os.path.join(root, "*.hdf"))

# hdfFiles = ["/Users/Char/googleDrive/tomoProject/MODIS_h08v05/MOD09GA.A2017173.h08v05.006.2017175031514.hdf"]
# bands = [bands[0]]


def writeToFile(fileName, pixels, ndvi, evi, evi2):
    print("Writing to " + fileName)
    hdfName = os.path.splitext(os.path.split(fileName)[1])[0]
    with open(fileName, "w") as file_out:
        file_out.write(hdfName + "\n")
        for bandName in pixels:
            file_out.write(bandName + "\n")
            file_out.write(str(pixels[bandName]) + "\n")
        file_out.write("NDVI\n")
        file_out.write(str(ndvi) + "\n")
        file_out.write("EVI\n")
        file_out.write(str(evi) + "\n")
        file_out.write("EVI2\n")
        file_out.write(str(evi2) + "\n")

for hdfFile in hdfFiles:
    hdfSD = SD(hdfFile, SDC.READ)
    selectedBands = [hdfSD.select(band) for band in bands]
    selectedPixels = {}
    for band in selectedBands:
        bandName = band.info()[0]
        resolution = band.attributes()["Nadir Data Resolution"] if "Nadir Data Resolution" in band.attributes() else defaultResolutionIfNoneFound
        startIndices = startIndicesByResolution[resolution]
        counts = countsByResolution[resolution]
        pixels = band.get(start=startIndices, count=counts)
        selectedPixels[bandName] = pixels
        print(bandName)
        print(pixels)
    ndvi_top = (selectedPixels["sur_refl_b01_1"] - selectedPixels["sur_refl_b02_1"]).astype(float)
    ndvi_bottom = (selectedPixels["sur_refl_b02_1"] + selectedPixels["sur_refl_b01_1"]).astype(float)
    ndvi =  ndvi_top / ndvi_bottom 
    print("ndvi", ndvi)

    evi_top = 2.5 * (selectedPixels["sur_refl_b02_1"] - selectedPixels["sur_refl_b01_1"])
    evi_bottom = selectedPixels["sur_refl_b02_1"] + (6 * selectedPixels["sur_refl_b01_1"]) + (7.5 * selectedPixels["sur_refl_b03_1"]) + 1
    evi = (evi_top / evi_bottom) / 10000.0
    print("evi", evi)

    evi2_top = 2.5 * (selectedPixels["sur_refl_b02_1"] - selectedPixels["sur_refl_b01_1"])
    evi2_bottom = selectedPixels["sur_refl_b02_1"] + (2.4 * selectedPixels["sur_refl_b01_1"]) + 1
    evi2 = (evi2_top / evi2_bottom) / 10000.0
    print("evi2", evi2)

    outputTextFile = os.path.splitext(hdfFile)[0] + ".txt"
    writeToFile(outputTextFile, selectedPixels, ndvi, evi, evi2)

# modisTest = SD(modisHDF, SDC.READ)

### list the available subdatasets
#pprint(modisTest.datasets())

##read the selected band dataset to an array
# band12 = modisTest.select('sur_refl_b01_1')
#band12read = band12.get()

#print(band12read)

### slice out the desired pixels from the array


#######
# test = band12.get(start=(180:181),count=(1134:1136))

# pprint(test)


### 
#modisDirectory = file.datasets()

#for idx,sds in enumerate(modisDirectory.keys()):
 #   print idx,sds