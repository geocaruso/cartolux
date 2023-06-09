#map.lux.grid.pop.R

#Add population to grid and compute share of each cell in each assigned
# communes based on population rather than surfaces.
# NB: we keep many to one allocation based on maximum surface of cell,
#  see map.lux.grid.R and resulting sf polygon: "lukm_Grid1km_LAU2_m2.gpkg"
#  
# There are 2 sources of population for 2021, which differ slightly:
#  STATEC but with a limitation to >30 individuals per cell
#  EUROSTAT grid but with population of borders cells including population from
#   bordering countries (leading to over 20 000 more people)
# We here use the EU one for demonstration since it is available online

#Grid Population ----

## Read our grid including cells allocation to communes.
##  dropping the matrix (cells x communes) of surfaces and intersection counts
Grid1km_LAU2_m2<-sf::st_read("data/Grid1km_LAU2_m2.gpkg")
Grid1km_LAU2<-Grid1km_LAU2_m2[,c("CELLCODE","maxm2LAU2","COMMUNE","m2_sh_of_LAU2")]

# Read Eurostat grid:
# Eurostat census grid 2021 downloaded from
#  https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat
# and externaly roughly cut around Luxembourg
grid_euro_lux0<-sf::st_read("data/grid_euro_lux.gpkg")
# GRID_ID is long form of the CELLCODE, we rebuild CELLCODE from it:
grid_euro_lux0$CELLCODE<-paste0("1km",
                               stringr::str_sub(grid_euro_lux0$GRD_ID, 24, 28),
                               stringr::str_sub(grid_euro_lux0$GRD_ID, 16, 20))
#get rid of geometry and former ID
grid_euro_lux<-sf::st_drop_geometry(grid_euro_lux0[,-1])
names(grid_euro_lux)[1] <- "Pop2021EU"

#merge Eurostat pop to our grid
Grid1km_LAU2_popEU<-merge(Grid1km_LAU2,grid_euro_lux)

source("R/ggplot.themap.R")

thd<-0 #threshold for no population
p0fi<-ggplot.themap(Grid1km_LAU2_popEU[Grid1km_LAU2_popEU$Pop2021EU>thd,],
                     "Pop2021EU",n=7, style="fisher",
                    cl.colours = rev(RColorBrewer::brewer.pal(7,"Spectral")))
p0fi

p0qt<-ggplot.themap(Grid1km_LAU2_popEU[Grid1km_LAU2_popEU$Pop2021EU>thd,],
                    "Pop2021EU",n=7,
                    cl.colours = rev(RColorBrewer::brewer.pal(7,"Spectral")))
p0qt

thd<-30 #threshold for no population
p30qt<-ggplot.themap(Grid1km_LAU2_popEU[Grid1km_LAU2_popEU$Pop2021EU>thd,],
                 "Pop2021EU",n=7,
                 cl.colours = rev(RColorBrewer::brewer.pal(7,"Spectral")))
p30qt

p30fi<-ggplot.themap(Grid1km_LAU2_popEU[Grid1km_LAU2_popEU$Pop2021EU>thd,],
                     "Pop2021EU",n=7, style="fisher",
                     cl.colours = rev(RColorBrewer::brewer.pal(7,"Spectral")))
p30fi


#Build allocation factor (share) according to population
agg_popEU_LAU2<-aggregate(list(SumPop2021EU=Grid1km_LAU2_popEU$Pop2021EU),
                          by=list(LAU2=Grid1km_LAU2_popEU$maxm2LAU2), FUN=sum)

merged2many<-merge(Grid1km_LAU2_popEU,agg_popEU_LAU2,
                  by.x="maxm2LAU2", by.y="LAU2", sort.x=FALSE, all.x=TRUE)
merged2many$Pop2021EU_sh_of_LAU2<-merged2many$Pop2021EU/merged2many$SumPop2021EU

#Reorder rows and rearrange columns
Grid1km_LAU2_Pop2021EU<-merged2many[match(Grid1km_LAU2_popEU$CELLCODE,merged2many$CELLCODE),]
Grid1km_LAU2_Pop2021EU<-Grid1km_LAU2_Pop2021EU[,c(2,1,3,4,5,6,8,7)]

#Mapping correspondance shares
pqt_sh<-ggplot.themap(Grid1km_LAU2_Pop2021EU[Grid1km_LAU2_Pop2021EU$Pop2021EU>0,],
                     "Pop2021EU_sh_of_LAU2",n=7, style="quantile",
                     cl.colours = rev(RColorBrewer::brewer.pal(7,"PiYG")),
                     main.title = 'Each 1km grid cell contribution to commune',
                     sub.title = "Based on Population 2021",
                     leg.title = "Population sh. (qt)")
pqt_sh

# and overlay with communes
LUX<-sf::st_read("data/Communes102_4326.gpkg")
lucom3035<-sf::st_transform(LUX,crs=sf::st_crs(Grid1km_LAU2_Pop2021EU))

pqt_sh_over<-pqt_sh+ggplot2::geom_sf(data=lucom3035, fill=NA)
pqt_sh_over

#save
sf::st_write(Grid1km_LAU2_Pop2021EU,"data/Grid1km_LAU2_Pop2021EU.gpkg", delete_dsn=TRUE)
write.csv(sf::st_drop_geometry(Grid1km_LAU2_Pop2021EU),"data/Grid1km_LAU2_Pop2021EU.csv")

#print
pdf("output/Lux_map_grid_pop.pdf")
print(p0fi)
print(p0qt)
print(p30fi)
print(p30qt)
print(pfi_sh)
print(pfi_sh_over)
dev.off()

