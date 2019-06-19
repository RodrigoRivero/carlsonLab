### 07 June 2019'

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(fuzzyjoin)
library(data.table)

# Import the text file of VIIRS reflectances
day271 = read.table("/Users/Char/Dropbox/radcalnet_june2019_stuff/2017_sunny_days/2017_fall/RVUS00_2017_271_v00.03.input.copy.txt", header=FALSE, row.names = NULL)

# Transpose the dataframe
tday271 <- as.data.frame(t(as.matrix(day271)))

# Export the transposed data frame to CSV
write.csv(tday271,"/Users/Char/Dropbox/radcalnet_june2019_stuff/2017_sunny_days/2017_fall/tday271.csv")

# Import the transposed Data Frame
csv271 <- read.csv("/Users/Char/Dropbox/radcalnet_june2019_stuff/2017_sunny_days/2017_fall/tday271.csv")

# View imported dataframe
View(csv271)

# Make the first row of data into the column headers
colnames(csv271) = csv271[1, ] # the first row will be the header
csv271 = csv271[-1, ]          # removing the first row.

# Rename the non-wavelength and reflectance columns
colnames(csv271)[2]<-"Year"
colnames(csv271)[3]<-"DOY_U"
colnames(csv271)[4]<-"UTC"
colnames(csv271)[5]<-"DOY_L"
colnames(csv271)[6]<-"Local"
colnames(csv271)[7]<-"P"
colnames(csv271)[8]<-"T"
colnames(csv271)[9]<-"WV"
colnames(csv271)[10]<-"O3"
colnames(csv271)[11]<-"AOD"
colnames(csv271)[12]<-"Ang"
colnames(csv271)[13]<-"Type"


#Make sure renamed columns showup
View(csv271)

# Remove the first column of old column headers that are no longer used
csv271 <- csv271[,-c(1) ]

# Melt the reflectance data from wide to long format
t2<- csv271 %>%
  gather(key="wavelength", value = "reflectance", -c("Year","DOY_U","UTC","DOY_L","Local","P","T","WV","O3","AOD","Ang","Type"))

#View to make sure it melted right
View(t2)

# Change the wavelength and reflectance bands to numeric
t2[, c("wavelength","reflectance")] <- sapply(t2[, c("wavelength","reflectance")], as.numeric)

# Import the CSV file of VIIRS bands
viirs_bands = read.csv("/Users/Char/Dropbox/radcalnet_june2019_stuff/viirs_bands.csv")

# View the viirs_bands
View(viirs_bands)


# Change the start and end fields of VIIRS_bands to numeric (because they are wavelength values)
viirs_bands[, c("Start","End")] <- sapply(viirs_bands[, c("Start","End")], as.numeric)

# Change the VIIRS.Band Field to character 
viirs_bands[, c("VIIRS.Band")] <- sapply(viirs_bands[, c("VIIRS.Band")], as.character)

# Fuzzy join the dataframe of bands and reflectance ranges to the Dataframe of daily radcats observations
t3<-fuzzy_left_join(t2, viirs_bands, 
                    by=c("wavelength"= "Start", "wavelength"="End"),
                    match_fun=list(`>=`, `<=`)) %>% 
  select(Year,DOY_U,UTC,DOY_L,Local,P,T,WV,O3,AOD,Ang,Type, wavelength, reflectance, VIIRS.Band)

# View to make sure the fuzzy join worked
View(t3)

# Create a new dataframe called t4 that omits observations that do not fall under a VIIRS band
t4<-na.omit(t3)

View(t4)


#plot wavelength VS reflectance 
ggplot(data=t3, aes(x=t3$wavelength, y=t3$reflectance )) + geom_point(size=1, aes( color=VIIRS.Band, group=UTC)) + xlab("Wavelength") + ylab("Reflectance") + scale_x_continuous(breaks=seq(0, 2400, 400),limits = c(0,2400)) + scale_y_continuous(breaks=seq(0.0, 0.6, 0.1),  limits = c(0.0,0.6))+theme_bw()



