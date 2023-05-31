#grideuro.lux
#Eurostat census grid 2021 downloaded from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat
#urban.atlas.lux
#urban atlas 2012 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012?tab=download
#urban atlas 2018 downloaded from https://land.copernicus.eu/local/urban-atlas/urban-atlas-2018?tab=download
#And unzipped in data folder

#1. Reading grid ----
#4668518 entities
#Remove from environment for next steps
#Preselection on sig needed grid_euro_lux
lukm_euro<-sf::st_read("data/Eurostat_Census-GRID_2021_v1-0/ESTAT_Census_2011_V1-0.gpkg")
grid_euro_lux<-sf::st_read("data/grid_euro_lux.gpkg")
names(grid_euro_lux)[2] <- "Pop"

#2. Intersecting with municipalities
# Subsetting to Lux territory----
#As one can see the eea data is buffered around Luxembourg.
# Many cells are not needed.
# Select only those 1km overlapping Luxembourg communes:----
LUX<-sf::st_read("data/Communes102_4326.gpkg")
lucom3035<-sf::st_transform(LUX,crs=sf::st_crs(grid_euro_lux))
lucom3035[,"com_m2"]<-sf::st_area(lucom3035) #adds surface of commune

lukm_itsct<-sf::st_intersects(grid_euro_lux, lucom3035, sparse=FALSE)
# Note this is a 9024 cells X 102 communes matrix (because of sparse=FALSE)
# Cells with rowSums >0 are thus overlapping and to be kept
grid_euro_lux[,"n_intscts"]<-rowSums(lukm_itsct) #number of intersecting communes by each cell
grid_euro_lux$n_intscts_f<-factor(grid_euro_lux$n_intscts)#number of intersecting communes by each cell as factor

grid_estat_lu = grid_euro_lux[grid_euro_lux$n_intscts>0,]
#dim(lukm) # 2794 grid cells

#Save as a gpkg for external use
sf::st_write(grid_estat_lu,"output/grid_estat_lu.gpkg", delete_dsn=TRUE) #Warning to be checked
#Save as a gpkg for R use
saveRDS(grid_estat_lu,"output/grid_estat_lu.RDS")


luintsection<-sf::st_intersection(grid_estat_lu,lucom3035) #intersects
luintsection[,"itsct_m2"]<-sf::st_area(luintsection) #adds surface of intersected
luintsection[,"share_of_com"]<-as.numeric(luintsection$itsct_m2/luintsection$com_m2) #surface share of commune
luintsection[,"share_of_cell"]<-as.numeric(luintsection$itsct_m2/1000000) #surface share of cell

##3.2. Wide----
# Keep shares in wide and unique corresp to build corresp table
wide_cell_LAU2_m2<-reshape2::dcast(sf::st_drop_geometry(luintsection),GRD_ID~LAU2,value.var = "itsct_m2")
wide_cell_LAU2_m2[is.na(wide_cell_LAU2_m2)] <- 0 #could use na rm later but won't work with col.max
rowSums(wide_cell_LAU2_m2[,2:103]) #to check all rows sum to 1
#OK except external borders OK
colSums(wide_cell_LAU2_m2[,2:103]) #to check sums are surface of communes
#OK see lucom3035$com_m2 (re-ordered)

##3.3. Select max LAU2----
# and add as a column, plus cells contribution (share)
maxm2LAU2<-names(wide_cell_LAU2_m2)[max.col(wide_cell_LAU2_m2[,2:103])+1]
wide_cell_LAU2_m2$maxm2LAU2<-maxm2LAU2 
#with frequency of LAU2s across cells compute 1/freq to get contribution share
# assumes equal contribution even is surface is not entirely in cell,
# to be consistent with each cell being allocated to a single LAU2
Each_share_of_LAU2<-data.frame(1/table(wide_cell_LAU2_m2$maxm2LAU2))
names(Each_share_of_LAU2)<-c("LAU2","sh_of_LAU2")
#Merge LAU2 shares to cells
m_w_s<-merge(wide_cell_LAU2_m2,Each_share_of_LAU2,by.x="maxm2LAU2",by.y="LAU2",)
m_w_s_com<-merge(m_w_s,sf::st_drop_geometry(lucom3035[,c("COMMUNE","LAU2")]),by.x="maxm2LAU2", by.y="LAU2")

##3.4. Save corresponding table and sd----
#rearrange (GRD_ID then maxLAU2 and commune name,
# then shares and then tabulate area matrix)
Grid1km_LAU2_m2_1<-m_w_s_com[,c(2,1,106,105,3:104)]
names(Grid1km_LAU2_m2_1)[4:105]<-paste0("m2_",names(Grid1km_LAU2_m2_1)[4:105])

#reorder with grid_estat_lu order
Grid1km_LAU2_m2<-Grid1km_LAU2_m2_1[match(grid_estat_lu$GRD_ID,Grid1km_LAU2_m2_1$GRD_ID),]
lukm_Grid1km_LAU2_m2_sf<-merge(grid_estat_lu,Grid1km_LAU2_m2)

#Carte de correspondance des cellules et des communes
source("R/ggplot.themap.R")
p1b<-ggplot2::ggplot(data=lukm_Grid1km_LAU2_m2_sf)+
  ggplot2::geom_sf(ggplot2::aes(fill= maxm2LAU2), show.legend = FALSE) +
  ggplot2::scale_fill_viridis_d(option = "turbo") +
  ggplot2::labs(title = "Correspondace des cellules aux communes", caption = "Source : Eurostat") +
  ggplot2::theme_light()
p1b

#Carte de correspondance des cellules et des communes (Meilleur rendu)
p2b<-ggplot2::ggplot(data=lukm_Grid1km_LAU2_m2_sf)+
  ggplot2::geom_sf(ggplot2::aes(fill= COMMUNE), show.legend = FALSE) +
  ggplot2::scale_fill_viridis_d(option = "turbo") +
  ggplot2::labs(title = "Correspondace des cellules aux communes", caption = "Source : Eurostat") +
  ggplot2::theme_light()
p2b


#4.1. Processing Urban Atlas ----
#Loading urban atlas datas 2012 and 2018
#All steps for both
#Select categories of urban
#Verifying crs
UA12<-sf::st_read("data/LU001L1_LUXEMBOURG_UA2012_revised_v021/Data/LU001L1_LUXEMBOURG_UA2012_revised_v021.gpkg")
UA18<-sf::st_read("data/LU001L1_LUXEMBOURG_UA2018_v013/Data/LU001L1_LUXEMBOURG_UA2018_v013.gpkg")
artifcode <-c("11100", "11210", "11220", "11230", "11240")
artif12<- UA12[UA12$code_2012 %in% artifcode,]
artif18<-UA18[UA18$code_2018 %in% artifcode,]
sf::st_crs(artif12)<- sf::st_crs(grid_estat_lu)
sf::st_crs(artif18)<- sf::st_crs(grid_estat_lu)

#4.2. Create degree based on artifcode middle of range#

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

#4.3. Intersecting with grid_estat_lu ----
#Urban areas multiplied by percent (degree)
artif12_int<-sf::st_intersection(artif12, grid_estat_lu)
artif18_int<-sf::st_intersection(artif18, grid_estat_lu)#Share of urban

artif12_int$surfartif <-sf::st_area(artif12_int)
artif12_int$surfdegree_m2 <- (artif12_int$surfartif*artif12_int$degree)

artif18_int$surfartif <-sf::st_area(artif18_int)
artif18_int$surfdegree_m2 <- (artif18_int$surfartif*artif18_int$degree)

#4.4. Intersecting with municipalities ----
#Select columns
#Verifying for next steps urban areas by commune and grid_id
#Aggregate and merge all areas by grid_id
luintsection_urban12<-sf::st_intersection(artif12_int, lucom3035)
luintsection_urban12<-luintsection_urban12[,c(13:24)]
luintsection_urban12$surfdegree_m2 <- as.numeric(luintsection_urban12$surfdegree_m2)
som_com<-aggregate(luintsection_urban12$surfdegree_m2, list(luintsection_urban12$COMMUNE), sum)
som_grid<-aggregate(luintsection_urban12$surfdegree_m2, list(luintsection_urban12$GRD_ID), sum)

luintsection_urban18<-sf::st_intersection(artif18_int, lucom3035)
luintsection_urban18<-luintsection_urban18[,c(13:24)]
luintsection_urban18$surfdegree_m2 <- as.numeric(luintsection_urban18$surfdegree_m2)
som_com<-aggregate(luintsection_urban18$surfdegree_m2, list(luintsection_urban18$COMMUNE), sum)
som_grid<-aggregate(luintsection_urban18$surfdegree_m2, list(luintsection_urban18$GRD_ID), sum)

sumsurfdegree12 <- aggregate(artif12_int$surfdegree_m2, list(GRD_ID = artif12_int$GRD_ID), sum)
names(sumsurfdegree12)[2]<-"sumsurfdegree"
sumsurfdegree12$sumsurfdegree <- as.numeric(sumsurfdegree12$sumsurfdegree)

sumsurfdegree18 <- aggregate(artif18_int$surfdegree_m2, list(GRD_ID = artif18_int$GRD_ID), sum)
names(sumsurfdegree18)[2]<-"sumsurfdegree"
sumsurfdegree18$sumsurfdegree <- as.numeric(sumsurfdegree18$sumsurfdegree)

lukmdegree12 <-merge(grid_estat_lu,sumsurfdegree12,by="GRD_ID",all.x=TRUE)
lukmdegree18 <-merge(grid_estat_lu,sumsurfdegree18,by="GRD_ID",all.x=TRUE)
#save as geopackage for future use
sf::st_write(lukmdegree12, "output/lukmdegree12.gpkg", delete_dsn=TRUE)
sf::st_write(lukmdegree18, "output/lukmdegree18.gpkg", delete_dsn=TRUE)


##5. Wide----
#Share by grid_id is insufficient
#wide to have all areas by commune and by grid_id
#Keep shares in wide
wide_cell_LAU2_ud12_m2<-reshape2::dcast(sf::st_drop_geometry(luintsection_urban12),GRD_ID~LAU2,value.var = "surfdegree_m2", fun.aggregate = sum)
wide_cell_LAU2_ud12_m2[is.na(wide_cell_LAU2_ud12_m2)] <- 0 #could use na rm later but won't work with col.max
rowSums(wide_cell_LAU2_ud12_m2[,2:103]) #to check all rows sum to 1
#OK except external orders OK
colSums(wide_cell_LAU2_ud12_m2[,2:103]) #to check sums are surface of communes
#OK see lucom3035$com_m2 (re-ordered)

wide_cell_LAU2_ud18_m2<-reshape2::dcast(sf::st_drop_geometry(luintsection_urban18),GRD_ID~LAU2,value.var = "surfdegree_m2", fun.aggregate = sum)
wide_cell_LAU2_ud18_m2[is.na(wide_cell_LAU2_ud18_m2)] <- 0 #could use na rm later but won't work with col.max
rowSums(wide_cell_LAU2_ud18_m2[,2:103]) #to check all rows sum to 1
#OK except external orders OK
colSums(wide_cell_LAU2_ud18_m2[,2:103]) #to check sums are surface of communes
#OK see lucom3035$com_m2 (re-ordered)

##5.1. Save corresponding table and sd----
names(wide_cell_LAU2_ud12_m2)[2:103]<-paste0("au_m2_",names(wide_cell_LAU2_ud12_m2)[2:103])
names(wide_cell_LAU2_ud18_m2)[2:103]<-paste0("au_m2_",names(wide_cell_LAU2_ud18_m2)[2:103])
#5.2. Save as csv
write.csv(wide_cell_LAU2_ud12_m2,"output/wide_cell_LAU2_ud12_m2.csv")
write.csv(wide_cell_LAU2_ud18_m2,"output/wide_cell_LAU2_ud18_m2.csv")

#5.3. merge and save to gpkg
lukm_Grid1km_LAU2_m2_sf12<-merge(lukm_Grid1km_LAU2_m2_sf,wide_cell_LAU2_ud12_m2, by="GRD_ID",all.x=TRUE)
lukm_Grid1km_LAU2_m2_sf18<-merge(lukm_Grid1km_LAU2_m2_sf,wide_cell_LAU2_ud18_m2, by="GRD_ID",all.x=TRUE, )
lukm_Grid1km_LAU2_m2_sf12[is.na(lukm_Grid1km_LAU2_m2_sf12)] <- 0
lukm_Grid1km_LAU2_m2_sf18[is.na(lukm_Grid1km_LAU2_m2_sf18)] <- 0
sf::st_write(lukm_Grid1km_LAU2_m2_sf12, "output/lukm_Grid1km_LAU2_m2_sf12.gpkg", delete_dsn=TRUE)
sf::st_write(lukm_Grid1km_LAU2_m2_sf18, "output/lukm_Grid1km_LAU2_m2_sf18.gpkg", delete_dsn=TRUE)
