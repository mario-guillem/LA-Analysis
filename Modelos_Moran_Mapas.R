"
En este archivo se han generado los modelos
GLM
GAM 
Estudio de residuos
"
library(sf)
boundary <- st_read("C:/Users/Mario/Desktop/predicciones/LAPD_Divisions.shp")
datos = read.csv("C:/Users/Mario/Desktop/Trabajo Espaciales/Crime_Data_from_2020_to_Present.csv")

casos_totales <- c(27285,27306,22991,27364,28305,33822,28524,29430,36428,31320,25619,31303,31893,34118,38025,33704,39085,43028,34889,28268,45198)
boundary$casos_totales <- casos_totales

boundary$inmigracion <- c(36.83,34.25,38.63,36.1,38.6,38.07,45.79,39.6,38.99,32.2,31.08,33.12,32.84,36.71,38.36,31.43,38.76,38.82,35.26,27.11,43.21)

boundary$ingresos <- c(80574,89726,73255,148662,193871,55511,51454,60541,142624,107253,63230,72812,79887,43294,71502,49302,116721,42140,53623,57964,41026)

boundary$desempleo <- c(4.58,4.17,5.64,4.08,4.48,6.39,7.09,5.26,9.31,8.92,6.67,7.32,4.09,8.22,9.68,8.75,9.18,10,9.05,7.71,11.81)

boundary$homelessness_pop <- c(272,212,78,711,75,155,80,95,250,194,73,519,434,103,239,544,189,854,632,156,6544)
boundary$precio_alquiler <- c(1739,2184,2407,3649,2832,2293,2145,1724,1768,1458,1604,1180,1520,1340,1495,1273,2552,1361,1492,1563,1851)
summary(datos$AREA.NAME)

cor(boundary$casos_totales, boundary$desempleo)
cor(boundary$casos_totales, boundary$homelessness_pop)
library(tidyverse)
summary(boundary$casos_totales)
"FIGURA DISTRIBUCIÓN CASOS"
ggplot() +
  geom_sf(data = boundary, aes(fill = casos_totales), color = "transparent")+
  scale_fill_gradient(low="moccasin", high="red")+theme_minimal()+
  labs(title="Casos por Área", caption = "Elaboración propia", fill="Casos totales")


# gam mgcv ----------------------------------------------------------------
hist(boundary$casos_totales)
# GAM con mgcv

# El paquete mgcv permite analizar un efecto espacial sin covariable

library(ggplot2)
library(mgcv)
library(ggplot2)
library(RColorBrewer)
adj_list <- st_touches(boundary, boundary)
names(adj_list) <- as.factor(boundary$APREC)
CAR_model <- mgcv::gam(casos_totales ~ s(as.factor(boundary$APREC), xt=list(nb=adj_list), bs='mrf'),
                       data=boundary, method='REML')



boundary$fitted <- predict(CAR_model)
ggplot(boundary, aes(fill=fitted)) + geom_sf(lwd=0.2) +
  scale_fill_gradient(low="moccasin", high="red")+ theme_minimal()+
  labs(title="Valores ajustados. Casos ~ Efecto Espacial", caption = "Elaboración propia", fill="Casos totales")
summary(CAR_model)

boundary$residuosESPACIAL <- boundary$casos_totales - boundary$fitted



ggplot(boundary,aes(x=casos_totales,y=fitted)) + 
  geom_point() + 
  geom_smooth(method=MASS::rlm,se = FALSE) + coord_equal()




# gam gamm4 --------------------------------------------------------------
"DESEMPLEO"
library(gamm4)
CAR_model1 <- gamm4(formula = casos_totales ~ desempleo+ s(as.factor(boundary$APREC), xt=list(nb=adj_list), bs='mrf'), data=boundary, REML = T)

boundary$fittedCOV <- predict(CAR_model1$gam)
ggplot(boundary, aes(fill=fittedCOV)) + geom_sf(lwd=0.2) +
  scale_fill_gradient(low="moccasin", high="red")+ theme_minimal()+
  labs(title="Valores ajustados. Casos ~ (Desempleo y Efecto Espacial)", caption = "Elaboración propia", fill="Casos totales")

boundary$residuosDESEMPLEO <- boundary$casos_totales - boundary$fittedCOV

options(scipen = 999)
summary(CAR_model$gam)
hist(CAR_model1$gam$residuals)

summary(CAR_model1$gam)
#SKID ROW ESTÁ EN CENTRAL ES POR LO QUE MÁS CRIMENES HAY.


# glm ---------------------------------------------------------------------



glm <- glm(casos_totales~ homelessness_pop +inmigracion+desempleo, family = poisson, data = boundary)
summary(glm)
glm$fitted.values
boundary$fittedGLM <- glm$fitted.values
ggplot(boundary, aes(fill=fittedGLM)) + geom_sf(lwd=0.2) +
  scale_fill_gradient(low="moccasin", high="red")

hist(glm$residuals)
library(RColorBrewer)
plot(boundary$casos_totales)




anova(glm, test = "Chi")




"HOMELESS"

library(gamm4)
CAR_model2 <- gamm4(formula = casos_totales ~ homelessness_pop+s(as.factor(boundary$APREC), xt=list(nb=adj_list), bs='mrf'), data=boundary, REML = T)

boundary$fittedCOV2 <- predict(CAR_model2$gam)
ggplot(boundary, aes(fill=fittedCOV2)) + geom_sf(lwd=0.2) +
  scale_fill_gradient(low="moccasin", high="red")+theme_minimal()+
  labs(title="Valores ajustados. Casos ~ (Homelessness y Efecto Espacial)", caption = "Elaboración propia", fill="Casos totales")


options(scipen = 999)
summary(CAR_model2$gam)


boundary$residuosHOMELESS <- boundary$casos_totales - boundary$fittedCOV2


res_modelo <- gamm4(formula = casos_totales ~ residuosDESEMPLEO + 
                      s(as.factor(boundary$APREC), xt=list(nb=adj_list), bs='mrf'), 
                    data=boundary, REML = T)
summary(res_modelo$gam)



CAR_model3 <- gamm4(formula = casos_totales ~ homelessness_pop+ desempleo + s(as.factor(boundary$APREC), xt=list(nb=adj_list), bs='mrf'), data=boundary, REML = T)

boundary$fittedCOV3 <- predict(CAR_model3$gam)
ggplot(boundary, aes(fill=fittedCOV3)) + geom_sf(lwd=0.2) +
  scale_fill_gradient(low="moccasin", high="red")+theme_minimal()

options(scipen = 999)
summary(CAR_model3$gam)

library(sf)
library(tmap)
library(spdep)


neighbours <- poly2nb(boundary)
neighbours

listw <- nb2listw(neighbours)
listw
"Casos"
moran.test(boundary$casos_totales, listw)
"Desempleo"
moran.test(boundary$desempleo, listw) 
"Poblacion homeless"
moran.test(boundary$homelessness_pop, listw)


#Salen significativos los residuos, por lo que hay algun componente que

mean(boundary$residuosDESEMPLEO)
mean(boundary$residuosESPACIAL)
mean(boundary$residuosHOMELESS)  
"menores residuos proporciona es el homelessness"
# moran lisa --------------------------------------------------------------
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

# centers the variable of interest around its mean
m.crime <- boundary$casos_totales - mean(boundary$casos_totales)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

# builds a data quadrant
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

