library(dismo)
Huemul <- gbif('Hippocamelus', 'bisulcus', down=TRUE)
colnames(Huemul)
Huemul <- gbif('Hippocamelus', 'bisulcus', down=TRUE)
colnames(Huemul)
Huemul2 <- Huemul %>% dplyr :: filter(!is.na(lat) & !is.na(lon))
library (tidyverse)
data("mtcars")
ggplot(mtcars, aes(x= wt, y= mpg))+                #definir base de datos
  geom_point(col = "red")+                       #como representarlo
  theme_grey()                        #configuración de apariencia


#Color

data("ChickWeight")
ggplot(ChickWeight, aes (x= Time, y = weight))+
  geom_point(aes(colour= Diet))

data(diamonds)             
ggplot(diamonds, aes(carat, price))+
  geom_point(aes(color=cut, alpha= cut))

ggplot(ChickWeight, aes(Time, weight))+
  geom_point(shape=21, fill= "blue", col="red", alpha=0.5)

data(iris)
ggplot(iris, aes(Species, Petal.Length))+
  geom_boxplot(fill="orange", col= "dark grey")+
  theme_minimal()
ggplot(iris, aes(Species, Petal.Length))+
  geom_violin(fill="orange", col= "dark grey")+
  theme_minimal()+
  coord_flip()

ggplot(iris, aes(Species, Petal.Length))+
  geom_jitter(fill="orange")+
  theme_minimal()

ggplot(iris, aes(Species, Petal.Length))+
  geom_point(fill="orange", position="jitter")+
  theme_minimal()

ggplot(iris, aes(Species, Petal.Length))+
  geom_violin(alpha= 1 , fill="orange")+
  geom_jitter(fill="red")+
  theme_minimal()
   
ggplot(iris, aes(Species, Petal.Length))+
  geom_violin(data = iris %>% filter(Species=="versicolor"), aes( Species, Petal.Length), alpha= 1 , fill="orange")+
  geom_jitter(shape= 21, fill="red")+
  theme_minimal()

#Dos variables continuas

ggplot(diamonds, aes(carat, price))+
  geom_point(aes(col= cut))

ggplot(diamonds, aes(carat, price))+
  geom_hex(bins= 10)

#trazar lineas de tendencia

ggplot(ChickWeight, aes(Time, weight))+
  geom_point(aes(col=Diet))+
  geom_smooth(method= lm)

ggplot(mtcars, aes(wt, mpg))+
  geom_point()+
  geom_smooth(method= lm)

ggplot(ChickWeight, aes(Time, weight, col=Diet))+
  geom_point()+
  geom_smooth(method= lm)

# stat_smooth para es una variante de geom smooth pero da mas posibilidades de agregar más variables
# hay mas control de a función lineal, ajustar una curva y definir la formula con la que se ha hecho el modelo.

ggplot(ChickWeight, aes(Time, weight, col=Diet))+
  geom_point()+
  stat_smooth(formula= y~x, method= lm)

#leer datos desde RDS

meteo <- readRDS("G:/Mi unidad/Semestre IV/Analisis y manipulacion de datos en R/TempHum.rds")

#para cambiar una variable para que me la tome como númerica

meteo <- meteo %>% 
  mutate(Mes=as.numeric(Mes))
sant <- meteo %>% filter(Ciudad_localidad== "Quinta Normal")
ggplot(sant, aes(Mes, Temperatura))+
  geom_point()+
  geom_smooth(method=lm)


ggplot(sant, aes(Mes, Temperatura))+
  geom_point()+
  geom_smooth(method=lm, formula= y~I(x^2) +x)

#tomar dos variables de la data

sant2 <- meteo %>% 
  filter(Ciudad_localidad=="Quinta Normal") %>% 
  gather(key= Var_ambient, value= valor, Temperatura, Humedad)

ggplot(sant2, aes(Mes, valor, col= Var_ambient))+
  geom_point()+
  stat_smooth(method="lm", formula= y ~I(x^2)+x)

#Trabajar con dos ciudades

sant_PA <- meteo %>% 
  filter(Ciudad_localidad %in% c("Quinta Normal", "Punta Arenas","Arica", "Valparaíso", "La Serena", "Vallenar""))
ggplot(sant_PA, aes(Mes, Temperatura, col= Ciudad_localidad))+
  geom_point()+
  stat_smooth(method="lm", formula= y ~ I(x^2)+x)

