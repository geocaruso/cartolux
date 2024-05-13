#The script computes the max distance from any point within each polygon of an sf.
#Value and computing time depend on the simplification level (resolution)
#used for digitizing since it computes the max distance after pairing
# all coordinates making the convex hull of a polygon.
# If convex hull is switched to FALSE then it computes it pairs all coordinates
#  of each original polygon
#For large territories or high resolution contexts, a simplify of polygons
# may still be required beforehand.
# Example is made for Luxembourg municipalities (102 communes).
# In this case with or without convex hull makes little difference but the
# use of convHull==TRUE adds a toll. Hence it is defaulted to false.

#Function that compute max internal distance, plus NS and SE ranges for comparison
intern.distances<-function(sf,idname,convHull=FALSE){
#sf must be a polygon
#idname is colname of unique id
idlst<-sf::st_drop_geometry(sf)[,idname]

list_distances<-lapply(idlst,function(x){
i<-sf[sf::st_drop_geometry(sf)[,idname]==x,]
if(convHull==TRUE) {
  i<-sf::st_convex_hull(i)
}

bbi<-sf::st_bbox(i)
NSdistance<-round(as.numeric(bbi[4]-bbi[2]))
EWdistance<-round(as.numeric(bbi[3]-bbi[1]))
coords_i<-sf::st_coordinates(i[1,])[,1:2]
Maxdistance<-round(max(dist(coords_i)))
return(cbind(NSdistance,EWdistance,Maxdistance))
}
)

df_internal_distance<-cbind(sf::st_drop_geometry(sf),
      do.call(rbind,list_distances))
return(df_internal_distance)
}

#Example:
#Get lat long Lux 102 communes and reproject into LUREF coordinates system (meters)
lux102_4326<-sf::st_read("data/Communes102_4326.gpkg")
lux102_2169<-sf::st_transform(lux102_4326, crs=2169)

lux102_internal_distance<-intern.distances(lux102_2169,"LAU2")

write.csv(lux102_internal_distance,"data/lux102_internal_distance.csv")
