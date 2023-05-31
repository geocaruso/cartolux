#giscoR.get.Lux.communes.R
#
#Use of giscoR to retrieve current and past vintages of the communes of Luxembourg 

#LUX118----
lux118_4326<-giscoR::gisco_get_communes(year="2001", country="Luxembourg")
#rename existing ids
# otherwise cannot export to gpkg because of ID or FID etc... gdal issue
names(lux118_4326)[1]<-"gisco.id"
names(lux118_4326)[11]<-"gisco.FID"
sf::st_write(lux118_4326,"data/lux118_4326.gpkg", append=FALSE) #at first run get a warning because database absent

p<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=lux118_4326, fill=NA,colour="red", linewidth=0.7)+
  ggplot2::theme_void()
p

#LUX116----
lux116_4326<-giscoR::gisco_get_lau(year="2011", country="Luxembourg")
names(lux116_4326)[1]<-"gisco.id"
names(lux116_4326)[10]<-"gisco.FID"
sf::st_write(lux116_4326,"data/lux116_4326.gpkg", append=FALSE) #at first run get a warning because database absent

p2<-p+
  ggplot2::geom_sf(data=lux116_4326, fill=NA,colour="blue",linewidth=0.5)
p2

#LUX102----
lux102_4326<-giscoR::gisco_get_lau(year="2020", country="Luxembourg")
names(lux102_4326)[1]<-"gisco.id"
names(lux102_4326)[10]<-"gisco.FID"
sf::st_write(lux102_4326,"data/lux102_4326.gpkg", append=FALSE) #at first run get a warning because database absent

p3<-p2+
  ggplot2::geom_sf(data=lux102_4326, fill=NA,colour="green",linewidth=0.3)
p3+ggplot2::ggtitle("Luxembourg: 118 (red), 116 (blue), 102 (green) communes",subtitle = "source: giscoR")
