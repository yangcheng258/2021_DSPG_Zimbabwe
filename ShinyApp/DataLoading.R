# MAPS
MAP_2017_91_T_o <- readOGR(dsn = "./data/shapefiles/91DistrictShapefiles", layer="zwe_admbnda_adm2_zimstat_ocha_20180911")
MAP_2017_60_T_o <- readOGR(dsn = "./data/shapefiles/60DistrictShapefiles", layer="gadm36_ZWE_2")
MAP_2017_10_T_o <- readOGR(dsn = "./data/shapefiles/ProvinceShapefiles", layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# MPI Data

# 2017 Data
MPI_2017_91_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District.csv')
MPI_2017_91_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District_Urban.csv')
MPI_2017_91_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District_Rural.csv')
MPI_2017_91_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District.csv')
MPI_2017_91_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District_Urban.csv')
MPI_2017_91_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District_Rural.csv')
MPI_2017_60_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District.csv')
MPI_2017_60_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District_Urban.csv')
MPI_2017_60_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District_Rural.csv')
MPI_2017_60_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District.csv')
MPI_2017_60_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District_Urban.csv')
MPI_2017_60_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District_Rural.csv')
MPI_2017_10_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province.csv')
MPI_2017_10_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province_Urban.csv')
MPI_2017_10_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province_Rural.csv')
MPI_2017_10_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province.csv')
MPI_2017_10_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province_Urban.csv')
MPI_2017_10_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province_Rural.csv')
MPI_2017_1_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National.csv')
MPI_2017_1_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National_Urban.csv')
MPI_2017_1_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National_Rural.csv')
MPI_2017_1_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National.csv')
MPI_2017_1_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National_Urban.csv')
MPI_2017_1_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National_Rural.csv')
# 2011 Data
MPI_2011_60_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District.csv')
MPI_2011_60_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District_Urban.csv')
MPI_2011_60_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District_Rural.csv')
MPI_2011_60_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District.csv')
MPI_2011_60_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District_Urban.csv')
MPI_2011_60_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District_Rural.csv')
MPI_2011_10_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province.csv')
MPI_2011_10_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province_Urban.csv')
MPI_2011_10_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province_Rural.csv')
MPI_2011_10_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province.csv')
MPI_2011_10_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province_Urban.csv')
MPI_2011_10_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province_Rural.csv')
MPI_2011_1_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National.csv')
MPI_2011_1_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National_Urban.csv')
MPI_2011_1_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National_Rural.csv')
MPI_2011_1_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National.csv')
MPI_2011_1_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National_Urban.csv')
MPI_2011_1_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National_Rural.csv')

## 91 District Processing
names = c("Beitbridge Rural"	,
          "Beitbridge Urban"	,
          "Bikita"          	,
          "Bindura Rural"   	,
          "Bindura"         	,
          "Binga"           	,
          "Bubi"            	,
          "Buhera"          	,
          "Bulawayo Urban"  	,
          "Bulilima"        	,
          "Muzarabani"      	,
          "Chegutu Rural"   	,
          "Chegutu"         	,
          "Chikomba"        	,
          "Chimamimani"     	,
          "Chinhoyi"        	,
          "Chipinge"        	,
          "Chipinge Urban"  	,
          "Chiredzi"        	,
          "Chiredzi Town"   	,
          "Chirumhanzu"     	,
          "Chitungwiza"     	,
          "Chivi"           	,
          "Epworth"         	,
          "Gokwe North"     	,
          "Gokwe South"     	,
          "Gokwe Centre"    	,
          "Goromonzi"       	,
          "Guruve"          	,
          "Gutu"            	,
          "Gwanda Rural"    	,
          "Gwanda"          	,
          "Gweru Rural"     	,
          "Gweru"           	,
          "Harare Urban"    	,
          "Harare Rural"    	,
          "Hurungwe"        	,
          "Hwange Rural"    	,
          "Hwange"          	,
          "Hwedza"          	,
          "Insiza"          	,
          "Kadoma"          	,
          "Kariba Rural"    	,
          "Kariba"          	,
          "Karoi"           	,
          "Kwekwe Rural"    	,
          "Kwekwe"          	,
          "Lupane"          	,
          "Makonde"         	,
          "Makoni"          	,
          "Mangwe"          	,
          "Marondera Rural" 	,
          "Marondera Urban" 	,
          "Masvingo Rural"  	,
          "Masvingo Urban"  	,
          "Matobo"          	,
          "Mazowe"          	,
          "Mberengwa"       	,
          "Mbire"           	,
          "Mhondoro-Ngezi"  	,
          "Mountt Darwin"   	,
          "Madzi"           	,
          "Murehwa"         	,
          "Mutare Rural"    	,
          "Mutare Urban"    	,
          "Mutasa"          	,
          "Mutoko"          	,
          "Mvuri"           	,
          "Mwezeni"         	,
          "Nkayi"           	,
          "Norton"          	,
          "Nyanga"          	,
          "Plumtree"        	,
          "Redcliff"        	,
          "Rusape"          	,
          "Rushinga"        	,
          "Ruwa Local Board"	,
          "Sanyati"         	,
          "Seke"            	,
          "Shamva"          	,
          "Shurugwi Rural"  	,
          "Shurugwi Urban"  	,
          "Tsholotsho"      	,
          "Umguza"          	,
          "Umzingwane"      	,
          "UMP"             	,
          "Victoria Falls"  	,
          "Zaka"            	,
          "Zwinba" 	,
          "Zvishavane Rural"	,
          "Zvishavane Urban")

MAP_2017_91_T_o@data[["ADM2_EN"]] <- names

# Renames the columns in the data to merge
colnames(MPI_2017_91_T_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_U_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_R_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_T_n)[2] <- "ADM2_EN"
colnames(MPI_2017_91_U_n)[2] <- "ADM2_EN"
colnames(MPI_2017_91_R_n)[2] <- "ADM2_EN"


# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
MAP_2017_91_U_o = MAP_2017_91_T_o
MAP_2017_91_R_o = MAP_2017_91_T_o
MAP_2017_91_T_n = MAP_2017_91_T_o
MAP_2017_91_U_n = MAP_2017_91_T_o
MAP_2017_91_R_n = MAP_2017_91_T_o

# Merges the Map data together
MAP_2017_91_T_o@data = merge(MAP_2017_91_T_o@data, MPI_2017_91_T_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_U_o@data = merge(MAP_2017_91_U_o@data, MPI_2017_91_U_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_R_o@data = merge(MAP_2017_91_R_o@data, MPI_2017_91_R_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_T_n@data = merge(MAP_2017_91_T_o@data, MPI_2017_91_T_n, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_U_n@data = merge(MAP_2017_91_U_o@data, MPI_2017_91_U_n, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_R_n@data = merge(MAP_2017_91_R_o@data, MPI_2017_91_R_n, by = c("ADM2_EN"), sort = FALSE)

## 60 District Maps

# Renames the columns in the data to merge
colnames(MPI_2017_60_T_o)[2] <- "NAME_2"
colnames(MPI_2017_60_U_o)[2] <- "NAME_2"
colnames(MPI_2017_60_R_o)[2] <- "NAME_2"
colnames(MPI_2017_60_T_n)[2] <- "NAME_2"
colnames(MPI_2017_60_U_n)[2] <- "NAME_2"
colnames(MPI_2017_60_R_n)[2] <- "NAME_2"

MAP_2017_60_T_o@data$NAME_2[47] = "Bulilima"
MAP_2017_60_T_o@data$NAME_2[50] = "Mangwe"
MAP_2017_60_T_o@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
MAP_2017_60_T_o@data$NAME_2[25] = "Hwedza"

MAP_2017_60_U_o = MAP_2017_60_T_o
MAP_2017_60_R_o = MAP_2017_60_T_o
MAP_2017_60_T_n = MAP_2017_60_T_o
MAP_2017_60_U_n = MAP_2017_60_T_o
MAP_2017_60_R_n = MAP_2017_60_T_o

MAP_2017_60_T_o@data = merge(MAP_2017_60_T_o@data, MPI_2017_60_T_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_U_o@data = merge(MAP_2017_60_U_o@data, MPI_2017_60_U_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_R_o@data = merge(MAP_2017_60_R_o@data, MPI_2017_60_R_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_T_n@data = merge(MAP_2017_60_T_o@data, MPI_2017_60_T_n, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_U_n@data = merge(MAP_2017_60_U_o@data, MPI_2017_60_U_n, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_R_n@data = merge(MAP_2017_60_R_o@data, MPI_2017_60_R_n, by = c("NAME_2"), sort = FALSE)

## Province Data

colnames(MPI_2017_10_T_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_U_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_R_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_T_n)[2] <- "ADM1_EN"
colnames(MPI_2017_10_U_n)[2] <- "ADM1_EN"
colnames(MPI_2017_10_R_n)[2] <- "ADM1_EN"

MAP_2017_10_U_o = MAP_2017_10_T_o
MAP_2017_10_R_o = MAP_2017_10_T_o
MAP_2017_10_T_n = MAP_2017_10_T_o
MAP_2017_10_U_n = MAP_2017_10_T_o
MAP_2017_10_R_n = MAP_2017_10_T_o

MAP_2017_10_T_o@data = merge(MAP_2017_10_T_o@data, MPI_2017_10_T_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_U_o@data = merge(MAP_2017_10_U_o@data, MPI_2017_10_U_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_R_o@data = merge(MAP_2017_10_R_o@data, MPI_2017_10_R_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_T_n@data = merge(MAP_2017_10_T_o@data, MPI_2017_10_T_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_U_n@data = merge(MAP_2017_10_U_o@data, MPI_2017_10_U_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_R_n@data = merge(MAP_2017_10_R_o@data, MPI_2017_10_R_n, by = c("ADM1_EN"), sort = FALSE)
