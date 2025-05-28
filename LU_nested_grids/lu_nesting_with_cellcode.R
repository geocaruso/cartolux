#lu_nesting_with_cellcode.R

#1km grid of Lux
lukm<-readRDS("data/lukm3035.RDS") 

source("R/nest.grid.R")
# nested200<-nest.grid(lukm,res=200) ###about 20min
# 
# nested200coords<-data.frame(sf::st_coordinates(nested200))
# origincoords200<-nested200coords[match(unique(nested200coords[,"L2"]),nested200coords[,"L2"]),c(1,2)]
# nested200$CELLCODE200m<-paste0("200mE",origincoords200[,1]/100,"N",origincoords200[,2]/100)
# 
# sf::st_write(nested200,"data/lunested200.gpkg",delete_dsn=TRUE) #export gpkg

#GPKG OF LU GRID 500 m with code and coordinates of origin and centre
nested500<-nest.grid(lukm,res=500)
nested500coords<-data.frame(sf::st_coordinates(nested500))
origincoords500<-nested500coords[match(unique(nested500coords[,"L2"]),nested500coords[,"L2"]),c(1,2)]
nested500$CELLCODE500m<-paste0("500mE",origincoords500[,1]/100,"N",origincoords500[,2]/100)
nested500$ORIGIN_X<-origincoords500[,1]
nested500$ORIGIN_Y<-origincoords500[,2]
nested500$CENTROID_X<-sf::st_coordinates(sf::st_centroid(nested500))[,1]
nested500$CENTROID_Y<-sf::st_coordinates(sf::st_centroid(nested500))[,2]
sf::st_write(nested500,"LU_nested_grids/lunested500.gpkg",delete_dsn=TRUE) #export gpkg
sf::st_write(nested500,"LU_nested_grids/lunested500.shp",delete_dsn=TRUE) #export shp

#GPKG OF LU GRID 200 m with code and coordinates of origin and centre
nested200<-nest.grid(lukm,res=200) #about 20min!!
nested200coords<-data.frame(sf::st_coordinates(nested200))
origincoords200<-nested200coords[match(unique(nested200coords[,"L2"]),nested200coords[,"L2"]),c(1,2)]
nested200$CELLCODE200m<-paste0("200mE",origincoords200[,1]/100,"N",origincoords200[,2]/100)
nested200$ORIGIN_X<-origincoords200[,1]
nested200$ORIGIN_Y<-origincoords200[,2]
nested200$CENTROID_X<-sf::st_coordinates(sf::st_centroid(nested200))[,1]
nested200$CENTROID_Y<-sf::st_coordinates(sf::st_centroid(nested200))[,2]
sf::st_write(nested200,"LU_nested_grids/lunested200.gpkg",delete_dsn=TRUE) #export gpkg
sf::st_write(nested200,"LU_nested_grids/lunested200.shp",delete_dsn=TRUE) #export shp

#GPKG OF LU GRID 100 m with code and coordinates of origin and centre
nested100<-nest.grid(lukm,res=100)# over 20min !!
nested100coords<-data.frame(sf::st_coordinates(nested100))
origincoords100<-nested100coords[match(unique(nested100coords[,"L2"]),nested100coords[,"L2"]),c(1,2)]
nested100$CELLCODE100m<-paste0("100mE",origincoords100[,1]/100,"N",origincoords100[,2]/100)
nested100$ORIGIN_X<-origincoords100[,1]
nested100$ORIGIN_Y<-origincoords100[,2]
nested100$CENTROID_X<-sf::st_coordinates(sf::st_centroid(nested100))[,1]
nested100$CENTROID_Y<-sf::st_coordinates(sf::st_centroid(nested100))[,2]
sf::st_write(nested100,"LU_nested_grids/lunested100.gpkg",delete_dsn=TRUE) #export gpkg
sf::st_write(nested100,"LU_nested_grids/lunested100.shp",delete_dsn=TRUE) #export shp

