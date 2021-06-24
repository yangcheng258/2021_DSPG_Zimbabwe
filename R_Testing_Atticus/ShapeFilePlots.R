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

if (TRUE) {
  ggplot(ZimMap8, aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = ZimMap@data$Deprivation, color = 'black'))
}


map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = isKingdom), colour = "black") + theme_void()
map
