"
En este archivo se encuentra parte del análisis espacial:

Diferentes LISA en función de la geometria (segmentacion en barrios o en divisiones de la policia)
Morans
Representaciones 
"

## Importar librerias
library(classInt)
library(maptools)
library(rgdal)
library(tidyr)
library(RColorBrewer)
library("spdep")
library(plyr)
library(tmap)
############################
"DATOS DIVIDIOS EN BARRIOS"
############################

## Importar Datos
districts <- readOGR(dsn = "C:/Users/Mario/Desktop/LAPD_Reporting_Districts.shp")
crime = read.csv("C:/Users/Mario/Desktop/Trabajo Espaciales/Crime_Data_from_2020_to_Present.csv")

head(districts@data, 5)
w <- table(crime$Rpt.Dist.No)
rep.dis <- as.data.frame(w)
length(unique(rep.dis$Var1))
head(rep.dis, 5)


districts@data <- merge(districts@data, rep.dis, by.x = "REPDIST", by.y = "Var1", all.x = TRUE)
districts$Freq[is.na(districts$Freq)] <- 0
length(districts$Freq)


var <- districts@data[,"Freq"]
breaks <- classIntervals(var, n = 9, style = "fisher")
my_colours <- rev(brewer.pal(9, "RdBu"))
plot(districts, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)],   
     axes = FALSE, border = NA)
legend(x = -118.7, y = 34, legend = leglabs(breaks$brks), fill = my_colours, bty = "n", cex = 0.6)

districts@data

"

En la autocorrelación espacial mide cómo influye la distancia en una variable concreta, en este caso la frecuencia
de delitos en un distrito. Según la primera ley geográfica de Tobler, cabe esperar que la mayoría de los fenómenos
geográficos presenten algún tipo de autocorrelación espacial. Esto suele ocurrir en los datos con factores humanos,
ya que las personas de características similares tienden a residir en barrios similares debido a una serie de razones,
como el precio de la vivienda, la proximidad a los lugares de trabajo y factores culturales.La autocorrelación espacial
puede representarse de dos maneras: Globalmente o Localmente. El modelo global crea una medida única que representa todo
el conjunto de datos, mientras que los modelos locales nos permiten explorar la agrupación espacial en toda la zona de LA.
En primer lugar, tenemos que asignar vecindades a cada uno de los distritos. Existen dos métodos para hacerlo: Rook o Queen.
Podemos trazar los vínculos entre los distritos vecinos para visualizar su distribución en el espacio.
"



 

neighbours <- poly2nb(districts, queen = F)
neighbours
plot(districts, border = 'lightgrey')
plot(neighbours, coordinates(districts), add=TRUE, col='red')


neighbours2 <- poly2nb(districts, queen = FALSE)
neighbours2
plot(districts, border = 'lightgrey')
plot(neighbours, coordinates(districts), add=TRUE, col='blue')


#MORAN TEST
listw <- nb2listw(neighbours)
listw
moran.test(districts$Freq, listw)
# p valor practicamente 0

"a. La distribución espacial de los valores altos y los valores bajos 
en el dataset está más agrupada espacialmente de lo que se esperaría si los procesos espaciales subyacentes fueran aleatorios.

Como el pvalor es prácticamente 0 rechazamos la hipotesis nula y por lo tanto SI QUE EXISTE AUTOCORRELACIÓN ESPACIAL.


"


moran <- moran.plot(districts$Freq, listw = nb2listw(neighbours, style = "B"),ylab="valores retardados", xlab = "Frecuencia por barrio")

#test local de moran
local <- localmoran(x = districts$Freq, listw = nb2listw(neighbours2, style = "B"))

moran.map <- cbind(districts, local)

"LISA"

quadrant <- vector(mode="numeric",length=nrow(local))


m.crime <- districts$Freq - mean(districts$Freq)     


m.local <- local[,1] - mean(local[,1])    


signif <- 0.1 


quadrant[m.crime >0 & m.local>0] <- 4  
quadrant[m.crime <0 & m.local<0] <- 1      
quadrant[m.crime <0 & m.local>0] <- 2
quadrant[m.crime >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

plot(districts,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend(x = -118.7, y = 34,legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")



library(sf)
boundary <- st_read("C:/Users/Mario/Desktop/predicciones/LAPD_Divisions.shp")
datos = read.csv("C:/Users/Mario/Desktop/Trabajo Espaciales/Crime_Data_from_2020_to_Present.csv")



library(sf)
library(tmap)
library(spdep)

neighbours2 <- poly2nb(boundary, queen = T)

neighbours2

moran <- moran.plot(boundary$casos_totales, listw = nb2listw(neighbours2, style = "W"))


local <- localmoran(x = boundary$casos_totales, listw = nb2listw(neighbours2, style = "W"))

moran.map <- cbind(boundary, local)

tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

quadrant <- vector(mode="numeric",length=nrow(local))


m.crime <- boundary$casos_totales - mean(boundary$casos_totales)     


m.local <- local[,1] - mean(local[,1])    


signif <- 0.1 


quadrant[m.crime >0 & m.local>0] <- 4  
quadrant[m.crime <0 & m.local<0] <- 1      
quadrant[m.crime <0 & m.local>0] <- 2
quadrant[m.crime >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

plot(boundary$geometry,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend(x = -118.7, y = 34,legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

