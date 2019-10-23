########### PROPENSITY SCORE MATCHING for NPPs and ConcessionsAll01_noRSPO_overlaps
library(tidyr)
library(dplyr)
library(stringr)
library(gtools)
library(MatchIt)
library(ggplot2)

###################################  Import Raw CSV Files ######################################## 

### Non-RSPO plantations from the concessionsALl_01 dataset
cAll01_CSV <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/cAll01_extract_30_sept.csv")

# CSV Dataset CSV of country IDs and areas for concessionsAll_01
cAll01_countryID <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/concessionsAll_01_nonRSPO_noOverlaps.csv")

### NPP Concession dataset
npp_CSV <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/npp_extract_0926.csv")

# NPP Dataset CSV of country IDs and areas 
npp_countryID <- read.csv("/Users/Char/Dropbox/char_HCV_table_data_fall2019/NPPs_forHCVRN_20190613_indo_byNPPforGEE.csv")


# View the Imported Files

View(npp_CSV)

View(cAll01_CSV)

View(npp_countryID)

# Remove area column from NPP CSV

npp_CSV$area<-NULL

# Join in the country and province information

npp_CSV <- left_join(npp_CSV, npp_countryID)

cAll01_CSV <- left_join(cAll01_CSV, cAll01_countryID)


# View the column names so we can start organizing them

colnames(npp_CSV)
colnames(cAll01_CSV)

############ Cleaning the NPP Table for Matching ###########################
# Remove un-needed columns
npp_CSV[ ,c("system.index" , "Shape_Area", "filename",".geo","Id")] <- list(NULL)

## Pad forest loss column name years with leading zeros
names(npp_CSV)<- sub("loss_(\\d{1}$)", "loss_0\\1", names(npp_CSV))

colnames(npp_CSV)

## Fix mis_spelled NPP__ID column
npp_CSV<- rename(npp_CSV, "unique_ID" = "NPP_ID")


## Re-order the shapefile info columns AND Re-order the forest loss columns so that they are in order
npp_CSV <- npp_CSV %>%
  select(unique_ID, name, code, area_type, supplybase, member, subsidiary, member_no, country, island, province, hcv_type, riparian, river, area, area_km2, elev, slope, city_dist, opl_dist, roads_dist, sort(everything()))


colnames(npp_CSV)

########### Concessions ALl 01 Table Cleaning ####################
colnames(cAll01_CSV)

# Remove un-needed columns
cAll01_CSV [ ,c("system.index", ".geo", "CERTUNIT","majoritySt","memberCoun","memberNumb", "STATUS", "Source", "Use")] <- list(NULL)

## Pad forest loss column name years with leading zeros
names(cAll01_CSV)<- sub("loss_(\\d{1}$)", "loss_0\\1", names(cAll01_CSV))

colnames(cAll01_CSV)

## change ConcAllID column to be the same as the npp file
cAll01_CSV <- rename(cAll01_CSV,"unique_ID" = "ConcAllID", "supplybase" ="Company", "code"="Code") 


## Re-order the shapefile info columns AND Re-order the forest loss columns so that they are in order
cAll01_CSV <- cAll01_CSV %>%
  select(unique_ID, code, supplybase,island, province, area, area_km2, elev, slope, city_dist, opl_dist, roads_dist,  sort(everything()))

colnames(cAll01_CSV)
View(cAll01_CSV)


# Remove other islands that are not kalimantan, sumatra, or papua

cAll01_CSV<- cAll01_CSV %>%
  filter(island != "") 


### Table Prep For Propensity Score Matching ###

# Add Columns to designate treatment (NPP =1 , NON-RSPO= 0 )

npp_CSV$rspo_npp <- 1
cAll01_CSV$rspo_npp<- 0


str(cAll01_CSV$unique_ID)
str(npp_CSV$unique_ID)

# Subset the two dataframes to only include common columns

npp_CSV<- npp_CSV %>%
  select(-c(supplybase, name, area_type, member, subsidiary, member_no, country, hcv_type, riparian, river, HCV1, HCV2, HCV3, HCV4, HCV5, HCV6, HCVRNpubDa, NPPNotDate, auditEndDa, auditStart, certBody, devType))

colnames(npp_CSV)

cAll01_CSV<-cAll01_CSV %>%
  select(-supplybase, -rspoMember)


# Join the two tables into one big dataset
npp_cAll_both<- full_join(npp_CSV, cAll01_CSV)

View(npp_cAll_both)

colnames(npp_cAll_both)

summary(npp_cAll_both)

str(npp_cAll_both)


str(npp_cAll_both2$f90)

npp_cAll_both<- npp_cAll_both %>%
  drop_na(f90_loss_01)

View(npp_cAll_both)

#mutate_at(vars(f90_defor_01), ~replace(., is.na(.), 0))


######### Calculate annual defor rates for 2001-2009


# Calculate area of forest by year

npp_cAll_both$f90_area_01 <-npp_cAll_both$f90 - npp_cAll_both$f90_loss_01
npp_cAll_both$f90_area_02 <-npp_cAll_both$f90_area_01 - npp_cAll_both$f90_loss_02
npp_cAll_both$f90_area_03 <-npp_cAll_both$f90_area_02 - npp_cAll_both$f90_loss_03
npp_cAll_both$f90_area_04 <-npp_cAll_both$f90_area_03 - npp_cAll_both$f90_loss_04
npp_cAll_both$f90_area_05 <-npp_cAll_both$f90_area_04 - npp_cAll_both$f90_loss_05
npp_cAll_both$f90_area_06 <-npp_cAll_both$f90_area_05 - npp_cAll_both$f90_loss_06
npp_cAll_both$f90_area_07 <-npp_cAll_both$f90_area_06 - npp_cAll_both$f90_loss_07
npp_cAll_both$f90_area_08 <-npp_cAll_both$f90_area_07 - npp_cAll_both$f90_loss_08
npp_cAll_both$f90_area_09 <-npp_cAll_both$f90_area_08 - npp_cAll_both$f90_loss_09
npp_cAll_both$f90_area_10 <-npp_cAll_both$f90_area_09 - npp_cAll_both$f90_loss_10
npp_cAll_both$f90_area_11 <-npp_cAll_both$f90_area_10 - npp_cAll_both$f90_loss_11
npp_cAll_both$f90_area_12 <-npp_cAll_both$f90_area_11 - npp_cAll_both$f90_loss_12
npp_cAll_both$f90_area_13 <-npp_cAll_both$f90_area_12 - npp_cAll_both$f90_loss_13
npp_cAll_both$f90_area_14 <-npp_cAll_both$f90_area_13 - npp_cAll_both$f90_loss_14
npp_cAll_both$f90_area_15 <-npp_cAll_both$f90_area_14 - npp_cAll_both$f90_loss_15
npp_cAll_both$f90_area_16 <-npp_cAll_both$f90_area_15 - npp_cAll_both$f90_loss_16



#Defor 2001

npp_cAll_both$f90_defor_01 <- npp_cAll_both$f90_loss_01/npp_cAll_both$f90

#Defor 2002
npp_cAll_both$f90_defor_02<-npp_cAll_both$f90_loss_02/ npp_cAll_both$f90_area_01

#Defor 2003
npp_cAll_both$f90_defor_03<-npp_cAll_both$f90_loss_03/npp_cAll_both$f90_area_02

#Defor 2004
npp_cAll_both$f90_defor_04<-npp_cAll_both$f90_loss_04/npp_cAll_both$f90_area_03 

#Defor 2005
npp_cAll_both$f90_defor_05<-npp_cAll_both$f90_loss_05/npp_cAll_both$f90_area_04

#Defor 2006
npp_cAll_both$f90_defor_06<-npp_cAll_both$f90_loss_06/npp_cAll_both$f90_area_05

#Defor 2007
npp_cAll_both$f90_defor_07<-npp_cAll_both$f90_loss_07/npp_cAll_both$f90_area_06

#Defor 2008
npp_cAll_both$f90_defor_08<-npp_cAll_both$f90_loss_08/npp_cAll_both$f90_area_07

#Defor 2009
npp_cAll_both$f90_defor_09<-npp_cAll_both$f90_loss_09/npp_cAll_both$f90_area_08
#Defor 2010
npp_cAll_both$f90_defor_10<-npp_cAll_both$f90_loss_10/npp_cAll_both$f90_area_09

#Defor 2011
npp_cAll_both$f90_defor_11<-npp_cAll_both$f90_loss_11/npp_cAll_both$f90_area_10

#Defor 2012
npp_cAll_both$f90_defor_12<-npp_cAll_both$f90_loss_12/npp_cAll_both$f90_area_11

#Defor 2013
npp_cAll_both$f90_defor_13<-npp_cAll_both$f90_loss_13/npp_cAll_both$f90_area_12

#Defor 2014
npp_cAll_both$f90_defor_14<-npp_cAll_both$f90_loss_14/npp_cAll_both$f90_area_13

#Defor 2015
npp_cAll_both$f90_defor_15<-npp_cAll_both$f90_loss_15/npp_cAll_both$f90_area_14

#Defor 2016
npp_cAll_both$f90_defor_16<-npp_cAll_both$f90_loss_16/npp_cAll_both$f90_area_15


npp_cAll_both2<- npp_cAll_both %>%
  drop_na(f90_defor_01)

View(npp_cAll_both2)
colnames(npp_cAll_both2)

npp_cAll_both2[170:185]<-npp_cAll_both2[170:185]*100


######## Calculate the percentage of forest cover in 2000

npp_cAll_both2$f90_percov_00<-(npp_cAll_both2$f90/npp_cAll_both2$area_km2)*100

summary(npp_cAll_both2$f90_percov_00)

write.csv(npp_cAll_both2,"/Users/Char/Dropbox/char_HCV_table_data_fall2019/npp_nonRSPO_forMatching.csv")

#npp_cAll_both$f90_defor_17 <-npp_cAll_both$f90_loss_17/(npp_cAll_both$f90 - npp_cAll_both$f90_loss_01 - npp_cAll_both$f90_loss_02-npp_cAll_both$f90_loss_03 -npp_cAll_both$f90_loss_04 - npp_cAll_both$f90_loss_05 - npp_cAll_both$f90_loss_06 - npp_cAll_both$f90_loss_07-npp_cAll_both$f90_loss_08 - npp_cAll_both$f90_loss_09 - npp_cAll_both$f90_defor_10 - npp_cAll_both$f90_defor_11 - npp_cAll_both$f90_defor_12 - npp_cAll_both$f90_defor_13 -npp_cAll_both$f90_defor_14 - npp_cAll_both$f90_defor_15 - npp_cAll_both$f90_defor_16)

#npp_cAll_both$f90_defor_18 <-npp_cAll_both$f90_loss_18/(npp_cAll_both$f90 - npp_cAll_both$f90_loss_01 - npp_cAll_both$f90_loss_02-npp_cAll_both$f90_loss_03 -npp_cAll_both$f90_loss_04 - npp_cAll_both$f90_loss_05 - npp_cAll_both$f90_loss_06 - npp_cAll_both$f90_loss_07-npp_cAll_both$f90_loss_08 - npp_cAll_both$f90_loss_09 - npp_cAll_both$f90_defor_10 - npp_cAll_both$f90_defor_11 - npp_cAll_both$f90_defor_12 - npp_cAll_both$f90_defor_13 -npp_cAll_both$f90_defor_14 - npp_cAll_both$f90_defor_15 - npp_cAll_both$f90_defor_16 - npp_cAll_both$f90_defor_17)





summary(npp_cAll_both$f90_defor_01)








##///////////////////OLD CODE/////////////////////////////////////////////

# One option is to use mixed sort when padding column numbers so that they are in order
#names(npp_CSV)<- mixedsort(sub("loss_(\\d{1}$)", "loss_0\\1", names(npp_CSV)))

# Another way to re-order the forest loss columns outside of tidyverse
#npp_CSV<-npp_CSV[,order(colnames(npp_CSV)[18:102])]

#npp_CSV<-npp_CSV[,c(names(npp_CSV)[1:17],sort(names(npp_CSV)[18:102]),names(npp_CSV)[103:158])]