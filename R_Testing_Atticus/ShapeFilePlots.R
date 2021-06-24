# IMPORTS

library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)

# SCRIPT


# Loads the raw shapefile
ZimMap = readOGR(dsn = "D:\\Virginia Tech\\DSPG\\2021_DSPG_Zimbabwe\\R_Testing_Atticus", layer="ZimMap")




stats = sample(1:100, length(districts), replace=TRUE)

fake_data <- matrix(c(districts, stats), ncol = 3)

View(fake_data)

# Merges the shapefile and the district data

ZimMap_tidy = left_join(ZimMap@data, fake_data, by="NAME_2")


# Plots the final map of Zimbabwe

if (FALSE) {
  ggplot(ZimMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(color = 'white', size = 0.5, fill = "orange") +
    coord_equal() + 
    theme_minimal()
}

