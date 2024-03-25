####Matrice_distances routières et euclidiennes entre les mairies des communes luxembourgeoises
#####en utilisant le package OSRM de OpenStreetMap####

####Chargement du package####
install.packages("osrm")
library(osrm)

####Chargement de la couche de points correspondant aux mairies des communes du Luxembourg
mairies_com_lux <- sf::read_sf("data/Mairies_W2_2018.shp")
names(mairies_com_lux)[2] <- "Mairie"
names(mairies_com_lux)[6] <- "Localité"

####Calcul de distances en voitures (en m) entre les 100 premières communes et les 100 premières communes####
matdist <- osrm::osrmTable(src = mairies_com_lux$geometry[1:100], dst = mairies_com_lux$geometry[1:100], measure = "distance", osrm.profile = "car")
dist <- as.data.frame(matdist$distances/1000)


####Calcul de distances en voitures (en m) entre les 2 dernières communes et les 2 dernières communes####
matdist_2 <- osrm::osrmTable(src = mairies_com_lux$geometry[101:102], dst = mairies_com_lux$geometry[101:102], measure = "distance")
dist_2 <- as.data.frame(matdist_2$distances/1000)

####Calcul de distances en voitures (en m) entre les 2 dernières communes et les 100 premières communes####
matdist_3 <- osrm::osrmTable(src = mairies_com_lux$geometry[101:102], dst = mairies_com_lux$geometry[1:100], measure = "distance")
dist_3 <- as.data.frame(matdist_3$distances/1000)

####Calcul de distances en voitures (en m) entre les 100 premières communes et les 2 dernières communes####
matdist_4 <- osrm::osrmTable(src = mairies_com_lux$geometry[1:100], dst = mairies_com_lux$geometry[101:102], measure = "distance")

####en km####
dist_4 <- as.data.frame(matdist_4$distances/1000)

####Fusionner les data frames####
####D'abord les lignes des 100 premières colonnes -> 102 lignes et les 100 colonnes####
dist_1_3 <- rbind(dist, dist_3)
####Puis les lignes des 2 dernières colonnes -> 102 lignes et les 2 dernières colonnes####
dist_4_2 <- rbind(dist_4, dist_2)
####Et les 102 colonnes avec les 102 lignes####
dist <- cbind(dist_1_3, dist_4_2)
#####Récupération des noms des communes
row.names(dist) <- mairies_com_lux$Mairie
colnames(dist) <- mairies_com_lux$Mairie

####Ecriture de la matrice####
write.csv(dist, "output/dist.csv")

####Distances euclidiennes

####Calcul de la distance euclidienne entre les communes####
disteucli <- sf::st_distance(mairies_com_lux$geometry)
####Mise en forme en tant que matrice####
matdisteucli <- as.data.frame.matrix(disteucli)
####En km####
matdisteucli <- (matdisteucli/1000)

#####Récupération des noms des communes
row.names(matdisteucli) <- mairies_com_lux$Marie
colnames(matdisteucli) <- mairies_com_lux$Marie

####Ecriture de la matrice####
write.csv(matdisteucli, "output/disteucli.csv")
