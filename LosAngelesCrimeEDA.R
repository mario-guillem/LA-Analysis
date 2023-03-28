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

# VICTIMAS POR GENERO -----------------------------------------------------

# En la documentacion del gobierno no se especifican el significado de los valores de h x, por lo que tendremos tan solo en cuenta F y M.
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

ggplot(casos_area, aes(x = "" , y = `summary(datos$AREA.NAME)`, fill = fct_inorder(rownames(casos_area)))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df3,
                   aes(y = pos, label = paste0(`summary(datos$AREA.NAME)`)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Barrio")) +
  theme_void()



ggplot(data=casos_area, aes(x=rownames(casos_area), y=casos_area$`summary(datos$AREA.NAME)`)) +
  geom_bar(stat="identity" , fill= rgb(0.1,0.4,0.5,0.7))+
  geom_text(aes(label=casos_area$`summary(datos$AREA.NAME)`), vjust=1.6, color="white", size=3.5)+
  xlab("Barrio")+ ylab("Casos")+
  theme_minimal()

