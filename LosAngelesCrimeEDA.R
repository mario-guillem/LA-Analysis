datos <- read.csv("C:/Users/Mario/Desktop/Trabajo Espaciales/Crime_Data_from_2020_to_Present.csv")
library(lubridate)
library(tidyverse)

datos$DATE.OCC <- as.Date(datos$DATE.OCC, format="%m/%d/%Y %I:%M:%S %p")



# Cambiamos las codificaciones de las etnias de las victimas.
nuevos_valores <- c("Other Asian", "Black", "Chinese", "Cambodian", "Filipino", "Guamanian", "Hispanic/Latin/Mexican", "American Indian/Alaskan Native", "Japanese", "Korean", "Laotian", "Other", "Pacific Islander", "Samoan", "Hawaiian", "Vietnamese", "White", "Unknown", "Asian Indian")
datos$Vict.Descent <- factor(datos$Vict.Descent, levels = c("A", "B", "C", "D", "F", "G", "H", "I", "J", "K", "L", "O", "P", "S", "U", "V", "W", "X", "Z"), labels = nuevos_valores)

# create a vector of variable names to convert to factors
var_names <- c("Vict.Sex", "Vict.Descent", "Vict.Age", "AREA.NAME", "Premis.Cd","Premis.Desc", "Weapon.Desc", "Crm.Cd.Desc", "Status.Desc")

# use a for loop to convert each variable to a factor
for (var in var_names) {
  datos[[var]] <- factor(datos[[var]])
}

summary(datos)

# VICITMAS POR GENERO -----------------------------------------------------

# En la documentaicon del gobierno no se especifican el significado de los valores de h x, por lo que tendremos tan solo en cuenta F y M.
genero <- table(datos$Vict.Sex)
genero
#eliminamos los valores mal metidos y nos quedamos solo con hombres y mujeres
genero <- genero[c(2,4)]
genero2 <- as.data.frame(genero)

options(scipen=999)
ggplot(genero2, aes(x=Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#fdbb84", "#2c7fb8"))+
  geom_text(aes(label=Freq), vjust=-0.5)+
  theme_minimal()+ theme(legend.position = "none")+
  xlab("")+ ylab("")+labs(title = " Distribución de género")
  


# CASOS POR AÑOS ----------------------------------------------------------


n20 <- nrow(filter(datos, year(datos$DATE.OCC)== 2020))
n21 <-nrow(filter(datos, year(datos$DATE.OCC)== 2021))
n22 <- nrow(filter(datos, year(datos$DATE.OCC)== 2022))
n23 <- nrow(filter(datos, year(datos$DATE.OCC)== 2023))

year <- c(2020,2021,2022,2023)
casos <- c(n20,n21,n22,n23)

casosdf <- data.frame(year,casos)
#HACER UNA TABLA.

library(gt)

# Crear la tabla utilizando kable()
gt_tbl <- gt(casosdf)

gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Número de crímenes en Los Ángeles") %>% 
  gt::cols_label(year = "Año",casos = "Nº de casos" )

# Show the gt Table
gt_tbl



# CASOS POR ÉTNIA ---------------------------------------------------------


summary(datos$Vict.Descent)
etnia <- c("Other Asian (Not China)","Black","Chinese","Cambodian","Filipino","Guamanian","Hispanic/Latin/Mexican American",
           "American Indian/Alaskan Native","Japanese","Korean","Laotian","Other","Pacific Islander","Samoan","Hawaiian",
           "Vietnamese","White","Asian Indian")
n <- c(14736,98034,2467,47,2771,48,208609,649,924,3555,44,53713,180,38,131,665,139839,326)

df_etnia <- data.frame(etnia, n )

df_ordenado <- df_etnia[order(df_etnia$n, decreasing = TRUE),]

df_ordenado <- df_ordenado[1:5,]


library(ggplot2)
library(ggrepel)
library(tidyverse)

# Get the positions
df2 <- df_ordenado %>% 
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))

ggplot(df_ordenado, aes(x = "" , y = n, fill = fct_inorder(etnia))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(n)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Étnia")) +
  theme_void()

# EDADES DE LAS VICTIMAS --------------------------------------------------

library(dplyr)
library(ggplot2)

datos$Vict.Age <- as.numeric(datos$Vict.Age)
summary(datos$Vict.Age)

summary(as.factor(datos$Vict.Age))
#Vemos como hay una gran cantidad de victimas que tienen 3 años, por lo que pensamos que se debe a un error de introduccion
# de datos en el sistema, por lo que a la hora de realizar el gráfico no tendremos en cuenta dichas observaciones.



datos %>% group_by(Vict.Age) %>% filter(Vict.Age > 3) %>% count(Vict.Age) %>%
  ggplot(aes(x = Vict.Age, y = n)) + geom_bar(stat = "identity", fill="#69b3a2")+
  scale_x_continuous(n.breaks = 20)+ xlab("Edad")+ ylab("Víctimas")+
  theme_minimal()


# CRIMENES POR AREA -------------------------------------------------------

casos_area <- as.data.frame(summary(datos$AREA.NAME))

df3 <- casos_area %>% 
  mutate(csum = rev(cumsum(rev(`summary(datos$AREA.NAME)`))), 
         pos = `summary(datos$AREA.NAME)`/2 + lead(csum, 1),
         pos = if_else(is.na(pos), `summary(datos$AREA.NAME)`/2, pos))
'
ggplot(casos_area, aes(x = "" , y = `summary(datos$AREA.NAME)`, fill = fct_inorder(rownames(casos_area)))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df3,
                   aes(y = pos, label = paste0(`summary(datos$AREA.NAME)`)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Barrio")) +
  theme_void()
'

ggplot(data=casos_area, aes(x=rownames(casos_area), y=casos_area$`summary(datos$AREA.NAME)`)) +
  geom_bar(stat="identity" , fill= rgb(0.1,0.4,0.5,0.7))+
  geom_text(aes(label=casos_area$`summary(datos$AREA.NAME)`), vjust=1.6, color="white", size=3.5)+
  xlab("Barrio")+ ylab("Casos")+
  theme_minimal()




# MAPAS -------------------------------------------------------------------

library(sf) #for spatial data
library(ggplot2) #for plotting
library(dplyr) #for data manipulation
library(RColorBrewer) #for diverging colour scheme
library(tidyverse) #for piping
library(ggmap) #for creating maps
library(png) #for importing inset map image
library(magick) #for adding inset image
library(ggpubr) #for multi-plot figures
library(cowplot) # for multi-plot figures
library(knitr) # for including graphics

LA <- st_read("C:/Users/Mario/Desktop/City_Boundaries.shp") #WGS84
LA_city <- filter(LA, CITY_LABEL == "Los Angeles")


#HAY QUE BORRAR TODAS LAS QUE TIENEN LAS LATITUDES Y LONGITUED A 0 AAAAAAAAAAAAA
datos <- datos %>% filter(LON != 0 | LAT != 0) 
ggplot() +
  # Add the LA boundary shapefile
  geom_sf(data=LA_city) +
  # Add the crime point data +
  #geom_point(data=LAcrime, mapping = aes(x=LON, y=LAT), color="red") +
  # Add hex binned layer
  geom_hex(data=datos,
           mapping = aes(x=LON, y=LAT), bins=15, color="black")+
  scale_fill_fermenter(n.breaks=10,palette = "RdYlBu")+
  # No theme to remove lat/long coord axis
  theme_void()


ggplot() +
  # Add the LA city boundary
  geom_sf(data=LA_city) +
  # Calculate 2D kernel density estimate and plot the contours
  geom_density_2d(data=datos,
                  mapping = aes(x=LON, y=LAT)) +
  # No theme to remove lat/long coord axis
  theme_void()


map1<- ggplot() +
  # Add LA city boundary
  geom_sf(data=LA_city) +
  # 2D KDE and plot contours
  stat_density_2d(data=datos,
                  geom = "polygon",
                  contour = TRUE,
                  aes(x=LON, y=LAT, fill = after_stat(level)),
                  # Make transparent
                  alpha = 0.6,
                  # Contour line colour
                  colour = "darkblue",
                  # 5 bins used as this map will be smaller in main geovis
                  bins = 5) +
  # Use colour-blind friendly colour palette and format legend labels
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       breaks = c(20, 30, 40, 50, 60),
                       labels = c("Low","","Med","","High"),
                       name = "Density (KDE)") +
  # No theme to remove lat/long coord axis
  theme_void() +
  # Add plot title
  ggtitle("Crimenes en la ciudad de Los Ángeles") +
  # Legend and title formatting
  theme(legend.position = c(0.10, 0.25),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size=9, face="bold",hjust = 0.5, vjust= 1.5),
        plot.margin = rep(unit(0,"null"),4),
        panel.spacing = unit(0,"null"))

map1


# LO ROJO ES EL SKID ROW AHI VAMOS A ESTUDIAR.

