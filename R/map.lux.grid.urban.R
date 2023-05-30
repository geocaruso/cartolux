#map.lux.grid.urban.R

#Add urban surfaces to grid and compute share of each cell in each
# previously assigned communes (usign total surface) based on urban surface
#Urban surface is built from Urban Atlas soil sealing percentage categories

#urban atlas 2012 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012?tab=download
#urban atlas 2018 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2018?tab=download
#And unzipped in data folder

UA12<-sf::st_read("data/EXT/LU001L1_LUXEMBOURG_UA2012_revised_v021.gpkg")
UA18<-sf::st_read("data/EXT/LU001L1_LUXEMBOURG_UA2018_v013.gpkg")
artifcode <-c("11100", "11210", "11220", "11230", "11240")
artif12<- UA12[UA12$code_2012 %in% artifcode,]
artif18<-UA18[UA18$code_2018 %in% artifcode,]
sf::st_crs(artif12)<- sf::st_crs(grid_estat_lu)
sf::st_crs(artif18)<- sf::st_crs(grid_estat_lu)
