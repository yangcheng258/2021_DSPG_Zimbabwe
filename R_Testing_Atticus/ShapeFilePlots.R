setwd("G:/My Drive/PhD/Internship/Zimbabwe/03_Git/2021_DSPG_Zimbabwe/R_Testing_Atticus")



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


### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap, region = "ADM2_EN")

## Might need to modify the code here, ask Sambath the structure of the data
stats  <-  sample(1:100, length(ZimMap), replace=TRUE)
fake_data <- matrix(c(ZimMap, stats), ncol = 3)
View(fake_data)



# Merges the shapefile and the district data
ZimMap_tidy  <-  left_join(ZimMap@data, fake_data, by="ADM2_EN")


# Plots the final map of Zimbabwe

if (FALSE) {
  ggplot(ZimMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(color = 'white', size = 0.5, fill = "orange") +
    coord_equal() + 
    theme_minimal()
}




