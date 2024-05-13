#The script computes the max distance from any point within each
#municipality. It depends on the simplification level used for digitizing
# since it computes the max distance after pairing all coordinates making
# a polygon.
# For use in other contexts, a simplify of polygons may be required beforehand.

#Get lat long Lux 102 communes and reproject into LUREF coordinates system (meters)
lux102_4326<-sf::st_read("data/Communes102_4326.gpkg")
lux102_2169<-sf::st_transform(lux102_4326, crs=2169)

#Compute max plus NS and SE ranges
list_distances<-lapply(lux102_2169$LAU2,function(x){
i<-lux102_2169[lux102_2169$LAU2==x,]
bbi<-sf::st_bbox(i)
NSdistance<-as.numeric(bbi[4]-bbi[2])
EWdistance<-as.numeric(bbi[3]-bbi[1])
coords_i<-sf::st_coordinates(i[1,])[,1:2]
Maxdistance<-max(dist(coords_i))
return(cbind(NSdistance,EWdistance,Maxdistance))
}
)
lux102_internal_distance<-cbind(sf::st_drop_geometry(lux102_2169),
      do.call(rbind,list_distances))

write.csv(lux102_internal_distance,"data/lux102_internal_distance.csv")
