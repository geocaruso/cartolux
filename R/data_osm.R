#map.lux.osm.R
#
#Demo of how to retrieve osm data from R, bothe vector data and tiles
#Demo for each municipality of Luxembourg
install.packages("osmdata")
library(osmdata)
library(dplyr)
#Data: 102 municipalities from geopackage WGS84----
LUX<-sf::st_read("data/comlux.shp")
LUX <- sf::st_transform(LUX, crs = 4326)

#Atlas with OSM vector data----
# Enrich our atlas after retrieving some vector features (buildings and tress)
#  as a sf 
 #May take a few minutes
for (i in 1:102) {
  iLUX<-LUX[i,] #we subset the i'th observation (municipality)
  # keeping all the attributes including the geometry
  iname<-LUX[i,"com"]
  
  iopq <- osmdata::opq(bbox = sf::st_bbox(iLUX)) #function from osmdata pkg
  iquery2<-osmdata::add_osm_feature(opq=iopq,key = "amenity", value="townhall")
  iosm2<-osmdata::osmdata_sf(iquery2)
  itownhall<-iosm2$osm_points #from list of objects, keeps only points of townhall
  itownhall[,"itsct"] <- sf::st_intersects(x = itownhall, y = iLUX, sparse = FALSE)
  
ipoly_mairie <- itownhall %>%
  group_by(itsct) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  sf::st_convex_hull()

ipoly_mairie <- sf::st_centroid(ipoly_mairie)

ipoly_mairie_in<-ipoly_mairie[sf::st_drop_geometry(ipoly_mairie[,"itsct",])==TRUE,]

ipoly_mairie_in <- sf::st_join(x = ipoly_mairie_in, y = iLUX)
list_mairie[i] <- list(ipoly_mairie_in)
}

all_mairies <- do.call(rbind, (list_mairie))
