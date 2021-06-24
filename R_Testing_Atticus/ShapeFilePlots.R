<<<<<<< HEAD
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


=======
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# IMPORTS

library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)

# SCRIPT

# Loads the raw shapefile
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")


summary(ZimMap)
length(ZimMap)
head(ZimMap@data)

districts <- ZimMap8@data[["ADM2_EN"]]

### you might want to check/replace the "districs" By Yang
stats = sample(1:100, length(districts), replace=TRUE)

fake_data <- matrix(c(districts, stats), ncol = 2)

colnames(fake_data) <- c("ADM2_EN", "Deprivation")


### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap8, region = "ADM2_EN")


# Merges the shapefile and the district data

ZimMap_tidy = left_join(ZimMap8@data, fake_data, by = 'ADM2_EN', copy = TRUE)
ZimMap_tidy = left_join(ZimMap8@data, ZimMap@data, copy = TRUE, by = 'ADM2_EN')

ZimMap8@data <- ZimMap_tidy

ZimMap8@data[["Deprivation"]] <- apply(ZimMap8@data[["Deprivation"]], 2, as.numeric)

# Plots the final map of Zimbabwe


ggplot(ZimMap8, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = 'black', size = 0.5) + 
  scale_fill_gradient(low='blue', high = 'red')



ggplot() + geom_polygon(data = ZimMap8, aes(x = long, y = lat, group = group, fill = 'orange'), colour = "black") + theme_void()
>>>>>>> c11f01b689fee1fc993e6a57bb9edd2d05666d28
