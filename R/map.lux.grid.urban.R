#map.lux.grid.urban.R

#Add urban surfaces to grid and compute share of each cell in each
# previously assigned communes (usign total surface) based on urban surface
#Urban surface is built from Urban Atlas soil sealing percentage categories

#urban atlas 2012 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012?tab=download
#urban atlas 2018 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2018?tab=download
#And unzipped in data folder within EXT (git ignored)

# Data----
# UA12<-sf::st_read("data/EXT/LU001L1_LUXEMBOURG_UA2012_revised_v021.gpkg")
UA18<-sf::st_read("data/EXT/LU001L1_LUXEMBOURG_UA2018_v013.gpkg")
artifcode <-c("11100", "11210", "11220", "11230", "11240")
artif12<- UA12[UA12$code_2012 %in% artifcode,]
artif18<-UA18[UA18$code_2018 %in% artifcode,]

Grid1km_LAU2_Pop2021EU<-sf::st_read("data/Grid1km_LAU2_Pop2021EU.gpkg")

sf::st_crs(artif12)<- sf::st_crs(Grid1km_LAU2_Pop2021EU) #both are actually EPSG3035 but written differently?
sf::st_crs(artif18)<- sf::st_crs(Grid1km_LAU2_Pop2021EU)

# Degrees based on artifcode middle of range----

artif12$degree <-NA
artif12[artif12$code_2012=="11100","degree"]<-0.9
artif12[artif12$code_2012=="11210","degree"]<-0.65
artif12[artif12$code_2012=="11220","degree"]<-0.40
artif12[artif12$code_2012=="11230","degree"]<-0.20
artif12[artif12$code_2012=="11240","degree"]<-0.05

artif18$degree <-NA
artif18[artif18$code_2018=="11100","degree"]<-0.9
artif18[artif18$code_2018=="11210","degree"]<-0.65
artif18[artif18$code_2018=="11220","degree"]<-0.40
artif18[artif18$code_2018=="11230","degree"]<-0.20
artif18[artif18$code_2018=="11240","degree"]<-0.05

# Intersecting with grid ----
artif12_int<-sf::st_intersection(artif12, Grid1km_LAU2_Pop2021EU)
artif18_int<-sf::st_intersection(artif18, Grid1km_LAU2_Pop2021EU)#Share of urban

artif12_int$surfartif <-sf::st_area(artif12_int)
artif12_int$surfdegree_m2 <- (artif12_int$surfartif*artif12_int$degree)

artif18_int$surfartif <-sf::st_area(artif18_int)
artif18_int$surfdegree_m2 <- (artif18_int$surfartif*artif18_int$degree)

