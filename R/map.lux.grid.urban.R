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
artif18_int<-sf::st_intersection(artif18, Grid1km_LAU2_Pop2021EU)

# Weighted urban surfaces per intersected----
artif12_int$surfartif <-as.numeric(sf::st_area(artif12_int)) #as.numeric to remove units otherwise further calculations difficult
artif12_int$surfdegree_m2 <- (artif12_int$surfartif*artif12_int$degree)

artif18_int$surfartif <-as.numeric(sf::st_area(artif18_int))#as.numeric to remove units otherwise further calculations difficult
artif18_int$surfdegree_m2 <- (artif18_int$surfartif*artif18_int$degree)

# Weighted urban surfaces per cell----
Grid12_surfdegree_m2<-aggregate(list(SumSurf2012_m2=artif12_int$surfdegree_m2),
                                by=list(CELLCODE=artif12_int$CELLCODE), FUN=sum )
Grid18_surfdegree_m2<-aggregate(list(SumSurf2018_m2=artif18_int$surfdegree_m2),
                                by=list(CELLCODE=artif18_int$CELLCODE), FUN=sum )
Grid_SumSurf_m2<-merge(Grid12_surfdegree_m2,Grid18_surfdegree_m2,all=TRUE) #merge 2 years together

#Merge to grid and replaces NA's by 0m2
Grid1km_LAU2_Pop2021EU_Surf1218<-merge(Grid1km_LAU2_Pop2021EU,Grid_SumSurf_m2, all.x=TRUE)
Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2012_m2[is.na(Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2012_m2)]<-0
Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2[is.na(Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2)]<-0


#Build allocation factor (share) according to urban surface
# ! given allocation of each cell to a single commune based on total surface !

agg_SumSurf_LAU2<-aggregate(list(LAU2Surf2012_m2=Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2012_m2,
                                 LAU2Surf2018_m2=Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2),
                            by=list(LAU2=Grid1km_LAU2_Pop2021EU_Surf1218$maxm2LAU2), FUN=sum)

merged2many<-merge(Grid1km_LAU2_Pop2021EU_Surf1218,agg_SumSurf_LAU2,
                   by.x="maxm2LAU2", by.y="LAU2", sort.x=FALSE, all.x=TRUE)
merged2many$Urban2012_sh_of_LAU2<-merged2many$SumSurf2012_m2/merged2many$LAU2Surf2012_m2
merged2many$Urban2018_sh_of_LAU2<-merged2many$SumSurf2018_m2/merged2many$LAU2Surf2018_m2



#Reorder rows and rearrange columns
Grid1km_LAU2_Pop2021EU_Surf1218<-merged2many[match(Grid1km_LAU2_Pop2021EU_Surf1218$CELLCODE,merged2many$CELLCODE),]
Grid1km_LAU2_Pop2021EU_Surf1218<-Grid1km_LAU2_Pop2021EU_Surf1218[,c(2,1,3:11,13,14,12)]

#Mapping correspondance shares
pqt_sh_urb<-ggplot.themap(Grid1km_LAU2_Pop2021EU_Surf1218[Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2>0,],
                      "Urban2018_sh_of_LAU2",n=7, style="quantile",
                      cl.colours = rev(RColorBrewer::brewer.pal(7,"PiYG")),
                      main.title = 'Each 1km grid cell contribution to commune',
                      sub.title = "Based on Urbanised surfaces 2021",
                      leg.title = "Urban 2018 sh. (qt)")
pqt_sh_urb

# and overlay with communes
LUX<-sf::st_read("data/Communes102_4326.gpkg")
lucom3035<-sf::st_transform(LUX,crs=sf::st_crs(Grid1km_LAU2_Pop2021EU_Surf1218))

pqt_sh_urb_over<-pqt_sh_urb+ggplot2::geom_sf(data=lucom3035, fill=NA)
pqt_sh_urb_over

#Mapping net density or land artifi per person 
Grid1km_LAU2_Pop2021EU_Surf1218$artifLand_capita18<-
   Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2/Grid1km_LAU2_Pop2021EU_Surf1218$Pop2021EU

subset1<-Grid1km_LAU2_Pop2021EU_Surf1218[(Grid1km_LAU2_Pop2021EU_Surf1218$Pop2021EU>0) & (Grid1km_LAU2_Pop2021EU_Surf1218$SumSurf2018_m2>0),]
artifLand_capita<-ggplot.themap(subset1,"artifLand_capita18",
                                n=7, style="quantile",
                                cl.colours = rev(RColorBrewer::brewer.pal(7,"Spectral")),
                          main.title = 'Artificialised residential land per capita 2021',
                          leg.title = "m2/inh.")
artifLand_capita

artifLand_capita_over<-artifLand_capita+
  ggplot2::geom_sf(data=lucom3035, fill=NA)
artifLand_capita_over

#printing
pdf("output/Lux_map_grid_urban.pdf")
print(pqt_sh_urb_over)
dev.off()
