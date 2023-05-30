
source("R/nest.grid.R")

#data
lukm<-readRDS("data/lukm3035.RDS")

#Example of downscaling 1km grid to 250m with 10 first Luxembourg cells
p<-ggplot2::ggplot()+ggplot2::geom_sf(data=lukm[1:10,],fill=NA,colour="blue",linewidth=1)
p

nested250<-nest.grid(lukm[1:10,],res=250)

p2<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=nested250, fill=NA)+
  ggplot2::geom_sf(data=lukm[1:10,],fill=NA,colour="blue",linewidth=1)
p2

#Example of attempt to downscaling 1km grid to 300m
nested300<-nest.grid(lukm,res=300) #Must return an error

#Run and save entire Lux country at 500m
nested500<-nest.grid(lukm,res=500)

p3<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=nested500, fill=NA)+
  ggplot2::geom_sf(data=lukm,fill=NA,colour="blue",linewidth=0.5)
p3
sf::st_write(nested500,"data/lunested500.gpkg",delete_dsn=TRUE)

#Run and save entire Lux country at 100m
#
#TIME CONSUMIN!!!!
nested100<-nest.grid(lukm,res=100) #BEWARE OVER 20minutes on MacBook pro 2,3 GHz Quad-Core Intel Core i7 
sf::st_write(nested100,"data/lunested100.gpkg")

#Example of downscaling 1km grid to 50m for Luxembourg and Esch cities only
Grid1km_LAU2_m2<-sf::st_read("data/Grid1km_LAU2_m2.gpkg")
Grid1km_LuxEsch<-Grid1km_LAU2_m2[Grid1km_LAU2_m2$COMMUNE %in% c("Esch-sur-Alzette","Luxembourg"),]
nested100_LuxEsch<-nest.grid(Grid1km_LuxEsch,res=100)

p4<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=nested100_LuxEsch, fill=NA)+
  ggplot2::geom_sf(data=lukm,fill=NA,colour="blue",linewidth=0.5)+
  ggplot2::coord_sf(xlim=sf::st_bbox(nested100_LuxEsch)[c(1,3)],
                    ylim=sf::st_bbox(nested100_LuxEsch)[c(2,4)])
p4

pdf("output/Lux_nested_grids.pdf")
print(p2)
print(p3)
print(p4)
dev.off()


