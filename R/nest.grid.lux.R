
source("R/nest.grid.R")

#data
lukm<-readRDS("data/lukm3035.RDS")

#Example of downscaling 1km grid to 250m with 10 first Luxembourg cells
p<-ggplot()+geom_sf(data=lukm[1:10,],fill=NA,colour="blue",linewidth=1)
p

nested250<-nest.grid(lukm[1:10,],res=250)

p2<-ggplot()+
  geom_sf(data=nested250, fill=NA)+
  geom_sf(data=lukm[1:10,],fill=NA,colour="blue",linewidth=1)
p2

#Example of attempt to downscaling 1km grid to 300m
nested300<-nest.grid(lukm,res=300) #Must return an error

#Run and save entire Lux country at 500m
nested500<-nest.grid(lukm,res=500)

p3<-ggplot()+
  geom_sf(data=nested500, fill=NA)+
  geom_sf(data=lukm,fill=NA,colour="blue",linewidth=0.5)
p3
sf::st_write(nested500,"data/lunested500.gpkg")

#Run and save entire Lux country at 100m
nested100<-nest.grid(lukm,res=100) #BEWARE OVER 20minutes on MacBook pro 2,3 GHz Quad-Core Intel Core i7 
sf::st_write(nested100,"data/lunested100.gpkg")
