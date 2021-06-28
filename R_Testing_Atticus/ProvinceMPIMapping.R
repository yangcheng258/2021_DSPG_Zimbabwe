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

# Loads in the shapefile
ZimMap <- readOGR(dsn = paste0(getwd(),"/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining
id <- ZimMap@data[["ADM1_EN"]]
MPIData = read.csv(file = 'ProvinceData.csv')

# Rename the district to id
colnames(MPIData)[1] <- "id"

### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap, region = "ADM1_EN")

# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, MPIData , by = c("id"))

# Plots the data
ggplot(datapoly, aes(x=long, y=lat, group = group)) +  geom_polygon(aes(fill = M0_k3, group = id)) + scale_fill_gradient(low='grey', high = 'red')
