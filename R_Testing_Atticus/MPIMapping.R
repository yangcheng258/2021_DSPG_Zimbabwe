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
library(maptools)
gpclibPermit()


# Loads the raw shapefile
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")
ProvinceMap <- readOGR(dsn = paste0(getwd(),"/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining
id <- ZimMap@data[["ADM2_EN"]]
MPIData = read.csv(file = 'MappingData.csv')

if (TRUE) {
  # Renaming the column discrepancies in the data
  ZimMap@data[["ADM2_EN"]][	1	] = 	"Beitbridge Rural"
  ZimMap@data[["ADM2_EN"]][	2	] = 	"Beitbridge Urban"
  ZimMap@data[["ADM2_EN"]][	3	] = 	"Bikita"
  ZimMap@data[["ADM2_EN"]][	4	] = 	"Bindura Rural"
  ZimMap@data[["ADM2_EN"]][	5	] = 	"Bindura"
  ZimMap@data[["ADM2_EN"]][	6	] = 	"Binga"
  ZimMap@data[["ADM2_EN"]][	7	] = 	"Bubi"
  ZimMap@data[["ADM2_EN"]][	8	] = 	"Buhera"
  ZimMap@data[["ADM2_EN"]][	9	] = 	"Bulawayo Urban"
  ZimMap@data[["ADM2_EN"]][	10	] = 	"Bulilima"
  ZimMap@data[["ADM2_EN"]][	11	] = 	"Muzarabani"
  ZimMap@data[["ADM2_EN"]][	12	] = 	"Chegutu Rural"
  ZimMap@data[["ADM2_EN"]][	13	] = 	"Chegutu"
  ZimMap@data[["ADM2_EN"]][	14	] = 	"Chikomba"
  ZimMap@data[["ADM2_EN"]][	15	] = 	"Chimamimani"
  ZimMap@data[["ADM2_EN"]][	16	] = 	"Chinhoyi"
  ZimMap@data[["ADM2_EN"]][	17	] = 	"Chipinge"
  ZimMap@data[["ADM2_EN"]][	18	] = 	"Chipinge Urban"
  ZimMap@data[["ADM2_EN"]][	19	] = 	'Chiredzi'
  ZimMap@data[["ADM2_EN"]][	20	] = 	"Chiredzi Town"
  ZimMap@data[["ADM2_EN"]][	21	] = 	"Chirumhanzu"
  ZimMap@data[["ADM2_EN"]][	22	] = 	"Chitungwiza"
  ZimMap@data[["ADM2_EN"]][	23	] = 	"Chivi"
  ZimMap@data[["ADM2_EN"]][	24	] = 	"Epworth"
  ZimMap@data[["ADM2_EN"]][	25	] = 	"Gokwe North"
  ZimMap@data[["ADM2_EN"]][	26	] = 	"Gokwe South"
  ZimMap@data[["ADM2_EN"]][	27	] = 	"Gokwe Centre"
  ZimMap@data[["ADM2_EN"]][	28	] = 	"Goromonzi"
  ZimMap@data[["ADM2_EN"]][	29	] = 	"Guruve"
  ZimMap@data[["ADM2_EN"]][	30	] = 	"Gutu"
  ZimMap@data[["ADM2_EN"]][	31	] = 	"Gwanda Rural"
  ZimMap@data[["ADM2_EN"]][	32	] = 	"Gwanda"
  ZimMap@data[["ADM2_EN"]][	33	] = 	"Gweru Rural"
  ZimMap@data[["ADM2_EN"]][	34	] = 	"Gweru"
  ZimMap@data[["ADM2_EN"]][	35	] = 	"Harare Urban"
  ZimMap@data[["ADM2_EN"]][	36	] = 	"Harare Rural"
  ZimMap@data[["ADM2_EN"]][	37	] = 	"Hurungwe"
  ZimMap@data[["ADM2_EN"]][	38	] = 	"Hwange Rural"
  ZimMap@data[["ADM2_EN"]][	39	] = 	"Hwange"
  ZimMap@data[["ADM2_EN"]][	40	] = 	"Hwedza"
  ZimMap@data[["ADM2_EN"]][	41	] = 	"Insiza"
  ZimMap@data[["ADM2_EN"]][	42	] = 	"Kadoma"
  ZimMap@data[["ADM2_EN"]][	43	] = 	"Kariba Rural"
  ZimMap@data[["ADM2_EN"]][	44	] = 	"Kariba"
  ZimMap@data[["ADM2_EN"]][	45	] = 	"Karoi"
  ZimMap@data[["ADM2_EN"]][	46	] = 	"Kwekwe Rural"
  ZimMap@data[["ADM2_EN"]][	47	] = 	"Kwekwe"
  ZimMap@data[["ADM2_EN"]][	48	] = 	"Lupane"
  ZimMap@data[["ADM2_EN"]][	49	] = 	"Makonde"
  ZimMap@data[["ADM2_EN"]][	50	] = 	"Makoni"
  ZimMap@data[["ADM2_EN"]][	51	] = 	"Mangwe"
  ZimMap@data[["ADM2_EN"]][	52	] = 	"Marondera Rural"
  ZimMap@data[["ADM2_EN"]][	53	] = 	"Marondera Urban"
  ZimMap@data[["ADM2_EN"]][	54	] = 	"Masvingo Rural"
  ZimMap@data[["ADM2_EN"]][	55	] = 	"Masvingo Urban"
  ZimMap@data[["ADM2_EN"]][	56	] = 	"Matobo"
  ZimMap@data[["ADM2_EN"]][	57	] = 	"Mazowe"
  ZimMap@data[["ADM2_EN"]][	58	] = 	"Mberengwa"
  ZimMap@data[["ADM2_EN"]][	59	] = 	"Mbire"
  ZimMap@data[["ADM2_EN"]][	60	] = 	"Mhondoro-Ngezi"
  ZimMap@data[["ADM2_EN"]][	61	] = 	'Mountt Darwin'
  ZimMap@data[["ADM2_EN"]][	62	] = 	"Madzi"
  ZimMap@data[["ADM2_EN"]][	63	] = 	"Murehwa"
  ZimMap@data[["ADM2_EN"]][	64	] = 	"Mutare Rural"
  ZimMap@data[["ADM2_EN"]][	65	] = 	"Mutare Urban"
  ZimMap@data[["ADM2_EN"]][	66	] = 	"Mutasa"
  ZimMap@data[["ADM2_EN"]][	67	] = 	"Mutoko"
  ZimMap@data[["ADM2_EN"]][	68	] = 	"Mvuri"
  ZimMap@data[["ADM2_EN"]][	69	] = 	"Mwezeni"
  ZimMap@data[["ADM2_EN"]][	70	] = 	"Nkayi"
  ZimMap@data[["ADM2_EN"]][	71	] = 	"Norton"
  ZimMap@data[["ADM2_EN"]][	72	] = 	"Nyanga"
  ZimMap@data[["ADM2_EN"]][	73	] = 	"Plumtree"
  ZimMap@data[["ADM2_EN"]][	74	] = 	"Redcliff"
  ZimMap@data[["ADM2_EN"]][	75	] = 	"Rusape"
  ZimMap@data[["ADM2_EN"]][	76	] = 	"Rushinga"
  ZimMap@data[["ADM2_EN"]][	77	] = 	"Ruwa Local Board"
  ZimMap@data[["ADM2_EN"]][	78	] = 	"Sanyati"
  ZimMap@data[["ADM2_EN"]][	79	] = 	"Seke"
  ZimMap@data[["ADM2_EN"]][	80	] = 	"Shamva"
  ZimMap@data[["ADM2_EN"]][	81	] = 	"Shurugwi Rural"
  ZimMap@data[["ADM2_EN"]][	82	] = 	"Shurugwi Urban"
  ZimMap@data[["ADM2_EN"]][	83	] = 	"Tsholotsho"
  ZimMap@data[["ADM2_EN"]][	84	] = 	"Umguza"
  ZimMap@data[["ADM2_EN"]][	85	] = 	"Umzingwane"
  ZimMap@data[["ADM2_EN"]][	86	] = 	"UMP"
  ZimMap@data[["ADM2_EN"]][	87	] = 	"Victoria Falls"
  ZimMap@data[["ADM2_EN"]][	88	] = 	"Zaka"
  ZimMap@data[["ADM2_EN"]][	89	] = 	"Zwinba"
  ZimMap@data[["ADM2_EN"]][	90	] = 	"Zvishavane Rural"
  ZimMap@data[["ADM2_EN"]][	91	] = 	"Zvishavane Urban"
}

  
# Rename the district to id
colnames(MPIData)[1] <- "id"

### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
library(broom)
ZimMap_fortified <- tidy(ZimMap, region = "ADM2_EN")

# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, MPIData , by = c("id"))

# Plots the data
ggplot(datapoly, aes(x=long, y=lat, group = group)) +  geom_polygon(aes(fill = M0_k7, group = id)) + scale_fill_gradient(low='grey', high = 'red')

