datos = read.csv("C:/Users/Mario/Desktop/Trabajo Espaciales/Crime_Data_from_2020_to_Present.csv")
library(tidyverse)

#transfer the data name
Homicidios<-c(110,113)
Violacion<-c(121,122,815,820,821)
Robo<-c(210,220)
AsaltoConViolencia<-c(230,231,235)
ViolenciaDeGenero<-c(626,627,647,763,928,930,236,250,251,761,926)
DelitoSimple<-c(435,436,437,622,623,624,625)
RoboVivienda<-c(310,320)
VehiculoRobado<-c(510,520,433)
RoboEnVehiculo<-c(330,331,410,420,421)
RoboAPersona<-c(350,351,352,353,450,451,452,453)

datos<- datos%>%  mutate(Crm.Cd = ifelse(str_detect(datos$Crm.Cd,paste(Homicidios, collapse = "|")),'Homicidio',
           ifelse(str_detect(Crm.Cd,paste(Violacion, collapse = "|")),'Violacion',
           ifelse(str_detect(Crm.Cd,paste(Robo, collapse = "|")),'Robo',
           ifelse(str_detect(Crm.Cd,paste(AsaltoConViolencia, collapse = "|")),'AsaltoConViolencia',
           ifelse(str_detect(Crm.Cd,paste(ViolenciaDeGenero, collapse = "|")),'ViolenciaDeGenero',
           ifelse(str_detect(Crm.Cd,paste(DelitoSimple, collapse = "|")),'DelitoSimple',
           ifelse(str_detect(Crm.Cd,paste(RoboVivienda, collapse = "|")),'RoboVivienda',
           ifelse(str_detect(Crm.Cd,paste(VehiculoRobado, collapse = "|")),'VehiculoRobado',
           ifelse(str_detect(Crm.Cd,paste(RoboEnVehiculo, collapse = "|")),'RoboEnVehiculo',
           ifelse(str_detect(Crm.Cd,paste(RoboAPersona, collapse = "|")),'RoboAPersona',
                                                                                 'OtroTipoDeRobo')))))))))))



nuevos_valores <- c("Other Asian", "Black", "Chinese", "Cambodian", "Filipino", "Guamanian", "Hispanic/Latin/Mexican", "American Indian/Alaskan Native", "Japanese", "Korean", "Laotian", "Other", "Pacific Islander", "Samoan", "Hawaiian", "Vietnamese", "White", "Unknown", "Asian Indian")
datos$Vict.Descent <- factor(datos$Vict.Descent, levels = c("A", "B", "C", "D", "F", "G", "H", "I", "J", "K", "L", "O", "P", "S", "U", "V", "W", "X", "Z"), labels = nuevos_valores)





top10<-datos%>%
  group_by(AREA.NAME, Rpt.Dist.No)%>%
  count()%>%
  ungroup()%>%
  filter(rank(n)>1173)

top10%>%
  mutate(Area = str_c(AREA.NAME,Rpt.Dist.No, sep='_'))%>%
  ggplot(aes(n, reorder(Area,n,fun=median), 
             fill =  AREA.NAME ))+
  geom_bar(stat='identity')+
  theme_bw()+
  labs(title = 'Top 10 of the areas that have the most numbers of the crime commited',
       subtitle='Hollywood, Central have three sub-areas having a lot of crimes',
       x='Number of the crime commited',
       y='Area',
       fill = 'Area')



total<-datos%>%
  mutate(age=cut_width(Vict.Age,5, boundary = 0))%>%
  group_by(AREA.NAME,Rpt.Dist.No, age, Vict.Sex, Vict.Descent)%>%
  count(Crm.Cd)%>%
  spread(Crm.Cd ,n, fill=0)%>%
  mutate(Hurto = RoboEnVehiculo +  mean(RoboAPersona + OtroTipoDeRobo,na.rm=T),
         Asalto = AsaltoConViolencia+ViolenciaDeGenero+DelitoSimple)

total<-total%>%
  filter(Vict.Descent!='X' & Vict.Sex != 'X' )


"Esto esta agrupado por cada zona de cada distrito"
total%>%
  group_by(Hurto)%>%
  count()%>%
  ggplot(aes(Hurto, log(n)))+
  geom_bar(stat='identity')+
  theme_bw()+xlim(0,100)
"parece una distribucion de poisson con lambda = 1"



total%>%
  group_by(Asalto)%>%
  count()%>%
  ggplot(aes(Asalto, log(n)))+
  geom_bar(stat='identity')+
  theme_bw()+xlim(0,50)
"esta parece lo mismo la verdad."

options(scipen = 999)
Pred1<-glm(Hurto~.,family=poisson, data=total[,-c(2,6:16)])
summary(Pred1)

Pred2<-glm(Asalto~.,family=poisson,data=total[,-c(2,7:17)])
summary(Pred2)





