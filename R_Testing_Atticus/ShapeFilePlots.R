# IMPORTS

library(ggplot2)
library(rgdal)

# SCRIPT


# Loads the raw shapefile
ZimMap = readOGR(dsn = "D:\\Virginia Tech\\DSPG\\2021_DSPG_Zimbabwe\\R_Testing_Atticus", layer="gadm36_ZWE_2")

districts = ZimMap@data[["NAME_2"]]

stats = sample(1:100, length(districts), replace=TRUE)

fake_data <- matrix(c(districts, stats), ncol = 3)

View(fake_data)

# Merges the shapefile and the district data



# Plots the final map of Zimbabwe

"
ggplot(ZimMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = 'white', size = 0.5, fill = "orange") +
  coord_equal() + 
  theme_minimal()
"

