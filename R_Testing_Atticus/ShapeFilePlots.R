setwd("G:/My Drive/PhD/Internship/Zimbabwe/03_Git/2021_DSPG_Zimbabwe/R_Testing_Atticus")



# IMPORTS

library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)

# SCRIPT

# Loads the raw shapefile
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")


summary(ZimMap8)
length(ZimMap8)
head(ZimMap8@data)


### you might want to check/replace the "districs" By Yang
stats = sample(1:100, length(districts), replace=TRUE)

fake_data <- matrix(c(districts, stats), ncol = 3)

View(fake_data)


### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
spdf_fortified <- tidy(ZimMap, region = "NAME")




# Merges the shapefile and the district data

ZimMap_tidy = left_join(ZimMap@data, fake_data, by="NAME_2")


# Plots the final map of Zimbabwe

if (FALSE) {
  ggplot(ZimMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(color = 'white', size = 0.5, fill = "orange") +
    coord_equal() + 
    theme_minimal()
}

