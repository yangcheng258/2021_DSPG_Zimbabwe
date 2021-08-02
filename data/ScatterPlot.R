# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/ShinyApp/data")


## 60 District Data

# Loads the shapefile
Dist_60_Map <- readOGR(dsn = paste0(getwd(),"/shapefiles/60DistrictShapefiles"), layer="gadm36_ZWE_2")

# Loads the district data
Dist_60_Total_2011 = read.csv("./MappingData/OriginalMPI/2011/2011_District.csv")
Dist_60_Urban_2011 = read.csv("./MappingData/OriginalMPI/2011/2011_District_Urban.csv")
Dist_60_Rural_2011 = read.csv("./MappingData/OriginalMPI/2011/2011_District_Rural.csv")

# Fixes four spelling changes in the shapefile
Dist_60_Map@data$NAME_2[47] = "Bulilima"
Dist_60_Map@data$NAME_2[50] = "Mangwe"
Dist_60_Map@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
Dist_60_Map@data$NAME_2[25] = "Hwedza"

# Renames the columns in the data to merge
colnames(Dist_60_Total_2011)[2] <- "NAME_2"
colnames(Dist_60_Urban_2011)[2] <- "NAME_2"
colnames(Dist_60_Rural_2011)[2] <- "NAME_2"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Dist_60_Total_Map_2011 = Dist_60_Map
Dist_60_Urban_Map_2011 = Dist_60_Map
Dist_60_Rural_Map_2011 = Dist_60_Map

# Merges the shapefiles with the data csv files 
Dist_60_Total_Map_2011@data = merge(Dist_60_Total_Map_2011@data, Dist_60_Total_2011, by = c("NAME_2"), sort = FALSE)
Dist_60_Urban_Map_2011@data = merge(Dist_60_Urban_Map_2011@data, Dist_60_Urban_2011, by = c("NAME_2"), sort = FALSE)
Dist_60_Rural_Map_2011@data = merge(Dist_60_Rural_Map_2011@data, Dist_60_Rural_2011, by = c("NAME_2"), sort = FALSE)

k_threshold = 3

UrbRurSelection = 1

map = switch(UrbRurSelection, Dist_60_Total_Map_2011, Dist_60_Urban_Map_2011, Dist_60_Rural_Map_2011)

M0_2011 = switch(k_threshold, map$M0_k1,
            map$M0_k2,
            map$M0_k3,
            map$M0_k4,
            map$M0_k5,
            map$M0_k6,
            map$M0_k7,
            map$M0_k8,
            map$M0_k9)

# Loads the district data
Dist_60_Total_2017 = read.csv("./MappingData/OriginalMPI/2017/2017_District.csv")
Dist_60_Urban_2017 = read.csv("./MappingData/OriginalMPI/2017/2017_District_Urban.csv")
Dist_60_Rural_2017 = read.csv("./MappingData/OriginalMPI/2017/2017_District_Rural.csv")

# Fixes four spelling changes in the shapefile
Dist_60_Map@data$NAME_2[47] = "Bulilima"
Dist_60_Map@data$NAME_2[50] = "Mangwe"
Dist_60_Map@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
Dist_60_Map@data$NAME_2[25] = "Hwedza"

# Renames the columns in the data to merge
colnames(Dist_60_Total_2017)[2] <- "NAME_2"
colnames(Dist_60_Urban_2017)[2] <- "NAME_2"
colnames(Dist_60_Rural_2017)[2] <- "NAME_2"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Dist_60_Total_Map = Dist_60_Map
Dist_60_Urban_Map = Dist_60_Map
Dist_60_Rural_Map = Dist_60_Map

# Merges the shapefiles with the data csv files 
Dist_60_Total_Map@data = merge(Dist_60_Total_Map@data, Dist_60_Total_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Urban_Map@data = merge(Dist_60_Urban_Map@data, Dist_60_Urban_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Rural_Map@data = merge(Dist_60_Rural_Map@data, Dist_60_Rural_2017, by = c("NAME_2"), sort = FALSE)

# Sets the k-threshold


map = switch(UrbRurSelection, Dist_60_Total_Map, Dist_60_Urban_Map, Dist_60_Rural_Map)

M0_2017 = switch(k_threshold, map$M0_k1,
                 map$M0_k2,
                 map$M0_k3,
                 map$M0_k4,
                 map$M0_k5,
                 map$M0_k6,
                 map$M0_k7,
                 map$M0_k8,
                 map$M0_k9)


M0_Comparison = data.frame(Dist_60_Total_2017$NAME_2, M0_2011, M0_2017)

colnames(M0_Comparison)[1] = "Name"

library(ggplot2)
library(ggrepel)
library(hrbrthemes)

create_scatter <- function(x_data, y_data, x_label, y_label, title) {
  return (ggplot(M0_Comparison, aes(x = x_data, y = y_data)) +
            geom_label_repel(aes(label = Name), size = 3, max.overlaps = 4,
                             
                             min.segment.length = unit(0, 'lines'),
                             nudge_y = 0.01) +
            geom_point(
              color= M0_2011,
              fill="#69b3a2",
              shape=22,
              alpha=1,
              size=2,
              stroke = 1
            ) +
            ggtitle(title) +
            xlab(x_label) +
            ylab(y_label) +
            theme_ipsum() +
            geom_abline()) 
  

}

create_scatter(M0_2011, M0_2017, "M0_2011", "M0_2017", "2017 to 2011 MPI Comparison")
  
scatter 

