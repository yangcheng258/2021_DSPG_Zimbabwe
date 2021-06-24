# your wd
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# clean the memory
rm(list=ls())

# IMPORTS
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)


# SCRIPT

# Loads the raw shapefile
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")


# summary(ZimMap)
# length(ZimMap)
# head(ZimMap@data)


# creating the fake MPI
id<- ZimMap@data[["ADM2_EN"]]
stats  <-  sample(1:100, length(id), replace=TRUE)
fake_data <-  cbind.data.frame( "id" = id, "Deprivation" = stats ) 


# position
### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap, region = "ADM2_EN")


# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, fake_data , by = c("id"))

ggplot(datapoly, aes(x=long, y=lat, group = group)) +  geom_polygon(aes(fill = Deprivation, group = id)) + scale_fill_gradient(low='grey', high = 'red')


