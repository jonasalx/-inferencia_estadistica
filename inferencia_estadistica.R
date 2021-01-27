#INFERENCIA ESTADISTICA
#JONATHAN  ULLOA

#install.packages("TeachingDemos")
#install.packages("car")
library(tidyverse)
library(TeachingDemos) 
library(car)

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



#Recuerde: * Probar supuesto de normalidad de los datos. 
shapiro.test(ventas$value)
#Como el p valor (0.7031) es mayor a alfa (0,05), no se rechaza la hipótesis nula (H0), por lo tanto, 
#la variable “value” presenta un comportamiento normal

library(car)
qqPlot(ventas$value)


#Conclusiones
#Las ventas medias no son superiores a 100 mil unidades semanales, 
#para aumentar la confianza se debe aumentar significativamente el tamaño de la muestra.





#Ejercicio 2
#Los datos del archivo historicos contiene 70 observaciones de 5 variables: la edad, género, peso y nicotina.
historicos  <- read_csv("historicos.csv")
unique(historicos$nicotina)


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

yes_nicotina<- historicos[historicos$nicotina=="Lo ha usado", ]
no_nicotina<- historicos[historicos$nicotina=="Nunca lo ha usado",
]

var.test(yes_nicotina$peso, calificaciones$despues, conf.level = 0.95)
#para nuestro test las varianzas son iguales (p-value > significancia)
#0.3503 > 0.05




t.test(x=yes_nicotina$peso, y=no_nicotina$peso, var.equal = FALSE, conf.level = 0.95)
#Existe una diferencia, pero no son significativas

#Recuerde en el caso c:
#probar los supuestos de normalidad de los datos

shapiro.test(historicos$peso)
#Como el p valor (0.3351) es mayor a alfa (0,05), no se rechaza la hipótesis nula (H0), por lo tanto, 
#la variable “peso” presenta un comportamiento normal

qqPlot(historicos$peso)


#Porbar la hipótesis de igualdad de las varianzas en caso que sean desconocidas.

var.test(yes_nicotina$peso, no_nicotina$peso)
#para nuestro test las varianzas son iguales (p-value > significancia)

#conclusión

#Con un 95% de confianza, las diferencias pormedio de pesos entre "fumadores y no fumadores" 
#se encuentra entre ( -6.185464  3.536684), como el cero está incluido,,, 
#se puede concluir que no hay diferencias en los pesos según sus habitos de fumador










#Ejercicio 3
#Una empresa tiene dos máquinas distintas para producir el mismo artículo, 
#y se extrae una muestra aleatoria simple de productos de cada una de ellas, 
#anotando cuáles son defectuosos. A partir de estos datos, recogidos en el archivo articulo 
#¿existe evidencia de que las proporciones de defectuosos de ambas máquinas son distintas?. 
#Asuma normalidad de los datos.

articulo  <- read_csv("articulo.csv")
head(articulo)

maq1 <- articulo[articulo$maquina == 1, ]
maq2 <- articulo[articulo$maquina == 2, ]

n1<- length(maq1$maquina)
n2<- length(maq2$maquina)

k1<- sum(maq1$defectuoso == 1)
k1

k2<- sum(maq2$defectuoso == 1)
k2

# (A)
#Pruebe usando un \(\alpha=0.05\) y luego pruebe usando \(\alpha=0.1\). Comente sus resultados.

prop.test(x=c(k1, k2), n=c(n1, n2), conf.level = 0.95)
#Con una confianza del 95% la diferencia de proporciones se encuentra (-0.01399051  0.25454995)
#si observamos el valor-p=0.07846 > 0.05 por lo tanto no se rechaza Ho


prop.test(x=c(k1, k2), n=c(n1, n2), conf.level = 0.9)
#Con una confianza del 90% la diferencia de proporciones se encuentra (0.004898297 0.235661143)
#si observamos el valor-p= 0.07846 < 0.1 por lo tanto se rechaza Ho


#la probabilidad de que ambar proporciones sean iguales es MUY baja
#Como el valor-P es mayor que  alpha no se rechaza la hipótesis nula 
#y se concluye que no hay evidencias suficientes para rechazar la hipótesis nula



# (B)
#¿ A partir de que valor de \(\alpha\) se rechazaría \(H_0\)?

na<- length(articulo$maquina)
nd<- sum(articulo$defectuoso == 1)

pbar = nd/na          # proporción muestral 
p0 = 1                # valor del parametro p 
n = na                # tamaño muestral 
z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 

# calculamos el valor crítico 

alpha = .05 
z.alpha = qnorm(1−alpha) 
−z.alpha               # critical value 

#a partir de 
#-1.644854


#Recuerde:
#platee la hipótesis a probar

Ho: p1 - p2 = 0
H1: p1 - p2 != 0


#Conclusión.





# (4)
#Ejercicio 4
#Para averiguar la eficacia del sistema de aprendizaje de un curso de inglés exprés, 
#se ha examinado a un grupo de estudiantes antes y después de asistir a dicho curso. 
#A partir de las calificaciones obtenidas (sobre 100 puntos), disponibles en el objeto calificaciones.

calificaciones <- read_csv("calificaciones.csv")
mean(calificaciones$antes)
mean(calificaciones$despues)

var(calificaciones$antes)
var(calificaciones$despues)

sd(calificaciones$antes)
sd(calificaciones$despues)

#Hipótesis
#H0: μ2-μ1 >= 5
#H1: μ2-μ1 < 5
  
shapiro.test(calificaciones$antes)
#Como el p valor (0.7031) es mayor a alfa (0,05), no se rechaza la hipótesis nula (H0), por lo tanto, 
#la variable “antes” presenta un comportamiento normal


shapiro.test(calificaciones$despues)
#Como el p valor (0.4349) es mayor a alfa (0,05), se rechaza la hipótesis nula (H0), por lo tanto, 
#la variable “antes” NO presenta un comportamiento normal

qqPlot(calificaciones$antes)
qqPlot(calificaciones$despues)


var.test(calificaciones$antes, calificaciones$despues)
#para nuestro test las varianzas no son iguales, para ambas significancias (p-value < significancia)


#¿Existe evidencia de que con la asistencia al curso la calificación media aumenta más de cinco puntos? 
#Use un 10% de significancia y luego un 5% de significancia. Comente los resultados.

t.test(calificaciones$antes, calificaciones$despues, mu=5, var.equal=F, conf.level = 0.90)
t.test(calificaciones$antes, calificaciones$despues, mu=5, var.equal=F, conf.level = 0.95)





#Con los resultados obtenidos en a. ¿A partir de que valor de α se rechazaría H0?



#Recuerde: Analizar supuestos antes de probar la hipótesis y no olvide plantear la hipótesis.



#OJO
#Para poder utilizar una prueba de t de student 
#a) Que se ajuste a una distribución normal, b) La independencia de los datos y c) La homogeneidad de varianzas,
