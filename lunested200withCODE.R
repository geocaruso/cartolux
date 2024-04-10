#lunested200withCODE.R

lukm<-readRDS("data/lukm3035.RDS")
nested200<-nest.grid(lukm,res=200) ###about 20min

nested200coords<-data.frame(sf::st_coordinates(nested200))
origincoords200<-nested200coords[match(unique(nested200coords[,"L2"]),nested200coords[,"L2"]),c(1,2)]
nested200$CELLCODE200m<-paste0("200mE",origincoords200[,1]/100,"N",origincoords200[,2]/100)

sf::st_write(nested200,"data/lunested200.gpkg",delete_dsn=TRUE) #export gpkg
