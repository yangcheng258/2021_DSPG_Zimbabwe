# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# clean the memory
rm(list=ls())

# IMPORTS
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
gpclibPermit()
library(maptools)

# Loads the raw shapefile
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")
ProvinceMap <- readOGR(dsn = paste0(getwd(),"/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining
id <- ZimMap@data[["ADM2_EN"]]
MPIData = read.csv(file = 'MappingData.csv')

if (FALSE) {
  # Renaming the column discrepancies in the data
  ZimMap@data[["ADM2_EN"]][1] = "Beitbridge Rural"
  ZimMap@data[["ADM2_EN"]][5] = "Bindura"
  ZimMap@data[["ADM2_EN"]][9] = "Bulawayo Urban"
  ZimMap@data[["ADM2_EN"]][11] = "Muzarabani"
  ZimMap@data[["ADM2_EN"]][61] = "Mountt Darwin"
  ZimMap@data[["ADM2_EN"]][81] = "Shurugwi Rural"
  ZimMap@data[["ADM2_EN"]][82] = "Shurugwi Urban"
  ZimMap@data[["ADM2_EN"]][86] = "UMP"
  ZimMap@data[["ADM2_EN"]][27] = "Gokwe Centre"
  ZimMap@data[["ADM2_EN"]][32] = "Gwanda"
  ZimMap@data[["ADM2_EN"]][42] = "Kadoma"
  ZimMap@data[["ADM2_EN"]][47] = "Kwekwe"
  ZimMap@data[["ADM2_EN"]][52] = "Marondera Rural"
  ZimMap@data[["ADM2_EN"]][62] = "Madzi"
  ZimMap@data[["ADM2_EN"]][77] = "Ruwa Local Board"
  ZimMap@data[["ADM2_EN"]][13] = "Chegutu"
  ZimMap@data[["ADM2_EN"]][43] = "Kariba Rural"
  ZimMap@data[["ADM2_EN"]][66] = "Mvuri"
  ZimMap@data[["ADM2_EN"]][4] = "Bindura Rural"
  ZimMap@data[["ADM2_EN"]][8] = "Bulawayo"
  ZimMap@data[["ADM2_EN"]][34] = "Gweru"
  ZimMap@data[["ADM2_EN"]][33] = "Gweru Rural"
  ZimMap@data[["ADM2_EN"]][38] = "Hwange"
  ZimMap@data[["ADM2_EN"]][37] = "Hwange Rural"
}

  
# Rename the district to id
colnames(MPIData)[1] <- "id"

### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap, region = "ADM2_EN")

# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, MPIData , by = c("id"))

# Plots the data
ggplot(datapoly, aes(x=long, y=lat, group = group)) +  geom_polygon(aes(fill = M0_k3, group = id)) + scale_fill_gradient(low='grey', high = 'red')
