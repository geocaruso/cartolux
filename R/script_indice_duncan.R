####Creation of Duncan focal index####
####Geoffrey Caruso and Yann Ferro####
####11/30/2023####
####geoffrey.caruso@uni.lu####
####yann.ferro@uni.lu####

####Duncan index focal/local####
####Calcul of segregation of each cells related to their own 8 neighbouring cells####
####Need headcount of natives or foreigners by example####
####Values from 1km² grids####
####Calcul of each proportion of native and foreigners in each cell divided by the total of the nine cells - allocated to the center cell####

library(terra)
library(ggplot2)

indice <- read.csv2("rp/indice_duncan.csv", fileEncoding = "Latin1", check.names = F, sep = ",")

####Classic Duncan index
####Total of each group of all cells####

sommeN <- sum(indice$Native_born)
sommeF <- sum(indice$NO_NPC_Born_Abroad)

indice$rapportN <- indice$Native_born/sommeN
indice$rapportF <- indice$NO_NPC_Born_Abroad/sommeF

#####Calcul of a classic national Duncan index####
duncan <- sum(abs(indice$rapportN-indice$rapportF))/2
duncan

####Used of a georeferenced grid####
head(indice)

####Keep only the latitude and longitude coordinates####

indice$y <- as.numeric(substr(indice$code_grids, 20, 26))
indice$x <- as.numeric(substr(indice$code_grids, 28, 34))

xyz_native <- cbind(indice$x, indice$y, indice$Native_born)
xyz_foreign <- cbind(indice$x, indice$y, indice$NO_NPC_Born_Abroad)

####Create a dataframe to get values###

df <- data.frame(x = indice$x, y = indice$y, native = indice$Native_born, foreign = indice$NO_NPC_Born_Abroad)

####Create raster for natives and foreign and check the rest is NA####
r_native <- terra::rast(xyz_native, type="xyz")

####Plot the raster####

plot(r_native)

r_foreign <- terra::rast(xyz_foreign, type="xyz")

####Plot the raster####

plot(r_foreign)

####Replace all NA by 0####

r_native_zero <- subst(r_native, NA, 0)

r_foreign_zero <- subst(r_foreign, NA, 0)

####Plot to see if NA have been replaced - see a difference with the previous####

plot(r_foreign_zero)

####See if 

sum(!is.na(values(r_native)))

####Focal sum for calculate the sum of the center cells natives/foreigners in a 3 by 3 window####

f_native <- focal(r_native_zero)
f_foreign <- focal(r_foreign_zero)

####Create filter for values of focal, need values of neighboring cells####
####Input 0 on each cells expected one to have 0 x focal sum et 1 x focal sum####

nord_ouest <- matrix(c(1,0,0,0,0,0,0,0,0), nrow = 3)
nord <-       matrix(c(0,0,0,1,0,0,0,0,0), nrow = 3)
nord_est <-   matrix(c(0,0,0,0,0,0,1,0,0), nrow = 3)
est <-        matrix(c(0,0,0,0,0,0,0,1,0), nrow = 3)
sud_est <-    matrix(c(0,0,0,0,0,0,0,0,1), nrow = 3)
sud <-        matrix(c(0,0,0,0,0,1,0,0,0), nrow = 3)
sud_ouest <-  matrix(c(0,0,1,0,0,0,0,0,0), nrow = 3)
ouest <-      matrix(c(0,1,0,0,0,0,0,0,0), nrow = 3)

####Get all values of focal for natives####

f_native_nord_ouest <- focal(r_native_zero, w = nord_ouest)
f_native_nord <- focal(r_native_zero, w = nord)
f_native_nord_est <- focal(r_native_zero, w = nord_est)
f_native_est <- focal(r_native_zero, w = est)
f_native_sud_est <- focal(r_native_zero, w = sud_est)
f_native_sud <- focal(r_native_zero, w = sud)
f_native_sud_ouest<- focal(r_native_zero, w = sud_ouest)
f_native_ouest<- focal(r_native_zero, w = ouest)

####Plot all the focals to see if there are correct####

plot(r_native_zero)
plot(f_native_nord_ouest)
plot(f_native_nord)
plot(f_native_nord_est)
plot(f_native_est)
plot(f_native_sud_est)
plot(f_native_sud)
plot(f_native_sud_ouest)
plot(f_native_ouest)
plot(r_native_zero)

####Get all values of focal for foreigners####

f_foreign_nord_ouest <- focal(r_foreign_zero, w = nord_ouest)
f_foreign_nord <- focal(r_foreign_zero, w = nord)
f_foreign_nord_est <- focal(r_foreign_zero, w = nord_est)
f_foreign_est <- focal(r_foreign_zero, w = est)
f_foreign_sud_est <- focal(r_foreign_zero, w = sud_est)
f_foreign_sud <- focal(r_foreign_zero, w = sud)
f_foreign_sud_ouest<- focal(r_foreign_zero, w = sud_ouest)
f_foreign_ouest<- focal(r_foreign_zero, w = ouest)

####Plot all the focals to see if there are correct####

plot(r_foreign_zero)
plot(f_foreign_nord_ouest)
plot(f_foreign_nord)
plot(f_foreign_nord_est)
plot(f_foreign_est)
plot(f_foreign_sud_est)
plot(f_foreign_sud)
plot(f_foreign_sud_ouest)
plot(f_foreign_ouest)
plot(r_foreign_zero)

####Value focal for natives and foreign to xyz - Keep only the first two columns of the focal####
####Natives####

df$native_window <- extract(f_native, df[, 1:2])[, "focal_sum"]

df$native_window_nord_ouest <- extract(f_native_nord_ouest, df[, 1:2])[, "focal_sum"]

df$native_window_nord <- extract(f_native_nord, df[, 1:2])[, "focal_sum"]

df$native_window_nord_est <- extract(f_native_nord_est, df[, 1:2])[, "focal_sum"]

df$native_window_est <- extract(f_native_est, df[, 1:2])[, "focal_sum"]

df$native_window_sud_est <- extract(f_native_sud_est, df[, 1:2])[, "focal_sum"]

df$native_window_sud <- extract(f_native_sud, df[, 1:2])[, "focal_sum"]

df$native_window_sud_ouest <- extract(f_native_sud_ouest, df[, 1:2])[, "focal_sum"]

df$native_window_ouest <- extract(f_native_ouest, df[, 1:2])[, "focal_sum"]

####Foreigners####

df$foreign_window <- extract(f_foreign, df[, 1:2])[, "focal_sum"]

df$foreign_window_nord_ouest <- extract(f_foreign_nord_ouest, df[, 1:2])[, "focal_sum"]

df$foreign_window_nord <- extract(f_foreign_nord, df[, 1:2])[, "focal_sum"]

df$foreign_window_nord_est <- extract(f_foreign_nord_est, df[, 1:2])[, "focal_sum"]

df$foreign_window_est <- extract(f_foreign_est, df[, 1:2])[, "focal_sum"]

df$foreign_window_sud_est <- extract(f_foreign_sud_est, df[, 1:2])[, "focal_sum"]

df$foreign_window_sud <- extract(f_foreign_sud, df[, 1:2])[, "focal_sum"]

df$foreign_window_sud_ouest <- extract(f_foreign_sud_ouest, df[, 1:2])[, "focal_sum"]

df$foreign_window_ouest <- extract(f_foreign_ouest, df[, 1:2])[, "focal_sum"]


####Sum of absolute values of each focal of native - each focal of foreigners####
somme_abs <- abs((df$native/df$native_window)-(df$foreign/df$foreign_window)) + 
                   abs((df$native_window_nord_ouest/df$native_window) -(df$foreign_window_nord_ouest/df$foreign_window)) +
                   abs((df$native_window_nord/df$native_window) -(df$foreign_window_nord/df$foreign_window)) + 
                   abs((df$native_window_nord_est/df$native_window) -(df$foreign_window_nord_est/df$foreign_window)) +
                   abs((df$native_window_est/df$native_window) -(df$foreign_window_est/df$foreign_window)) + 
                   abs((df$native_window_sud_est/df$native_window) -(df$foreign_window_sud_est/df$foreign_window)) + 
                   abs((df$native_window_sud/df$native_window) -(df$foreign_window_sud/df$foreign_window)) +
                   abs((df$native_window_sud_ouest/df$native_window) -(df$foreign_window_sud_ouest/df$foreign_window)) + 
                   abs((df$native_window_ouest/df$native_window) -(df$foreign_window_ouest/df$foreign_window))

####Divide by the sum of the values from focals####
df$focal_duncan <- somme_abs/2

sum(df$focal_duncan>1, na.rm=TRUE)

####Merge the results by x and y coordinates of the Duncan index to the original dataframe####

indice_duncan_focal <- merge(indice, df, by = c("x", "y"), all.x = TRUE)

####Transform x and y in point####

df$geom <- sprintf("POINT(%s %s)", indice$x, indice$y)

####Tranform in sf object####

sf_point_duncan <- sf::st_as_sf(df, wkt = "geom")


####Plot the Duncan index - with NA####
ggplot()+
  geom_sf(data = sf_point_duncan, aes(col = focal_duncan))+
  scale_color_gradient(na.value = "red", high = "darkblue", low = "lightgreen")+
  theme_bw()

####Write your Duncan index#### GG####

write.csv(indice_duncan_focal, "rp/indice_duncan_focal.csv")
