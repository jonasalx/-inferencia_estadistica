#INFERENCIA ESTADISTICA
#JONATHAN  ULLOA

library(tidyverse)
#install.packages("TeachingDemos")
#install.packages("car")
library(TeachingDemos) 

#Ejercicio 1
#Las ventas semanales (en miles de unidades) de un producto en 16 supermercados con características similares 
#se recogen en el archivo ventas. Si se sabe que el nivel de ventas se distribuye normal:

ventas  <- read_csv("ventas.csv")
media <- mean(ventas$value)
sd <- sd(ventas$value)

# (A)
#Encuentre un intervalo de confianza al 95% para las ventas promedio semanales del producto. 
#¿ Se podría decir que las ventas medias son superiores a 100 mil unidades?

t.test(ventas$value, conf.level = 0.95)
# [ 97.39079 , 99.10921 ]

#para validar aun mas las ventas medias... no son superiores a 100 mil unidades
#especifico la media que quiero poner a prueba
t.test(ventas$value,  mu=100)


# (B)
#Encuentre un intervalo de confianza al 94% para la desviación estandar de las ventas semanales.
#Comente su relsultado.


z.test(ventas$value, sd=sd, conf.level = 0.94) 
# [ 97.49183 , 99.00817 ]


# (C)
#¿Cuál es el tamaño optimo que se debe considerar para estimar el numero promedio de ventas semanales 
#si se quiere una confianza del 90% y un margen de error de a lo más del 20% (e=0.2)?, 
#como no se tiene la varianza poblacional considere la varianza muestral.

alpha<-0.1
zstar = qnorm(1-alpha/2) 
sigma <- 1.61                    #sd ?? 1.61
E = 0.2                       #margen error
zstar^2 ∗ sigma^2/ E^2 

#175.326
# Se debe considerar un tamaño muestral de 175 con una confianza del 90% y un margen de error de 0.2



#Recuerde: * Probar supuesto de normalidad de los datos. * Conclusiones
shapiro.test(ventas$value)


library(car)
qqPlot(ventas$value)





#Ejercicio 2
#Los datos del archivo historicos contiene 70 observaciones de 5 variables: la edad, género, peso y nicotina.
historicos  <- read_csv("historicos.csv")
unique(historicos$nicotina)

# (A)
#Se quiere saber si existen diferencias significativas en el peso de estas personas segun sus hábitos 
#de uso de nicotina. Use un nivel de significancia del 5%.

# (B)
#Encuentre el tamaño optimo para estimar la proporción de personas con hábitos de consumo de nicotina 
#si se requiere de un nivel de confianza del 95% con un error en la estimación de e=0.1. 
#Use la proporción muestral para determinar n. 

n<- length(historicos$nicotina)
x = sum(historicos$nicotina == "Lo ha usado")
p <-x/n
p


alpha<-0.05
zstar = qnorm(1-alpha/2) 
Error = 0.1                       #margen error
n <- zstar^2 ∗ p ∗ (1−p) / Error^2 

#93.21417
# El tamaño optimo para estimar la proporción de personas con hábitos de consumo es de 93


# (C)
#Se quiere saber si existen diferencias significativas en el peso de estas personas segun sus hábitos 
#de uso de nicotina. Use un nivel de significancia del 5%.

#Recuerde en el caso c:
#probar los supuestos de normalidad de los datos
#Porbar la hipótesis de igualdad de las varianzas en caso que sean desconocidas.
#conclusión