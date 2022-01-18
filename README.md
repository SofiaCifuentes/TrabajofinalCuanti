## Sesion R -- 01/03/2022

### 1. Cálculos básicos

### 1.1 Suma
1+1

### 1.2 Calcular el 15% de $1.900
0.15*1900

### 2. Instalar estos Paquetes 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("mlogit")
install.packages("stargazer")
install.packages("rsq")
install.packages("sjPlot")
install.packages("dslabs")

### 2.1 Cargar desde la biblioteca estos Paquetes
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(mlogit)
library(stargazer)
library(rsq)
library(sjPlot)
library(dslabs)

## 2. Encuesta CEP
### 2.1 Link: https://www.cepchile.cl/cep/encuestas-cep/encuestas-2010-2021/estudio-nacional-de-opinion-publica-n-85-septiembre-2021

### 2.2 Importar base de datos
library(haven)
base_85 <- read_sav("encuesta_cep_ago2021/base_85.sav")
View(base_85)

# 2.3 Variable numérica
base_85[sapply(base_85, is.numeric)] <- lapply(base_85[sapply(base_85, is.numeric)], as.factor)

## 3. Análisis descriptivo

### 3.1 Frecuencias

### Variable Inicial: elec_pres_1
table(base_85$elec_pres_1)

### Variable dependiente: elec_pres_1 en porcentaje 
table(base_85$elec_pres_1)
table_1 <- table(base_85$elec_pres_1)
prop.table(table_1)


### 3.2 Ejemplo tablas de contingencia: elec_pres_1 y sexo
## Visualizacion en RStudio
sjt.xtab( base_85$elec_pres_1, # Variable 1
          base_85$sexo, # Variable 2
          var.labels = c("Presidente", "Sexo"), # Nombres de las variables
          show.exp = TRUE)

## Visualizacion en word
sjt.xtab(base_85$elec_pres_1, #filas
         base_85$sexo, #columnas
         file = "1.doc")

### Ejemplo tablas de contingencia: elec_pres_1 y sexo % Columnas
sjt.xtab(base_85$elec_pres_1, #filas
         base_85$sexo, #columnas
         show.col.prc = T, file = "2.doc") 



### Ejemplo tablas de contingencia: elec_pres_1 y sexo % Filas y Columnas
sjt.xtab(base_85$elec_pres_1, #filas
         base_85$sexo, #columnas
         show.col.prc = T, show.row.prc = T, file = "3.doc")

### Mi análisis

### Ejemplo tablas de contingencia: elec_pres_1 y sexo % Filas y Columnas
sjt.xtab(base_85$info_hogar_31, #filas
         base_85$sexo, #columnas
         show.col.prc = T, show.row.prc = T, file = "4.doc")

### Mi trabajo: tabla de contingencia confianza CC y sexo
sjt.xtab(base_85$confianza_6_y, #filas
         base_85$sexo, #columnas
         show.col.prc = T, show.row.prc = T, file = "5.doc")


#### 3.3 Gráficos 

mi_factor <- factor(base_85$elec_pres_1)


#### 3.3.1 Gráfico de barras
plot(mi_factor, main = "Diagrama de barras")

plot(mi_factor,base_85$sexo, main = "Diagrama de barras")

#### 3.3.2 Diagrama de dispersión

library(ggplot2)

ggplot(base_85, aes(x = base_85$bienestar_3, y = base_85$bienestar_2)) +
  geom_point(colour = 4)

## 4. Test de Correlación. Ejemplo
table(base_85$bienestar_2)

sjt.xtab(base_85$bienestar_2, base_85$sexo, file = "7.doc")

library(dplyr)
install.packages("ggpubr")
library("ggpubr")
cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$percepcion_1_b), method=c("pearson", "kendall", "spearman"))

cor.test(as.numeric(base_85$bienestar_3), as.numeric(base_85$sexo), method=c("pearson", "kendall", "spearman"))

cor.test(as.numeric(base_85$bienestar_3), as.numeric(base_85$gse), method=c("pearson", "kendall", "spearman"))

cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$gse), method=c("pearson", "kendall", "spearman"))

cor.test(as.numeric(base_85$bienestar_2), as.numeric(base_85$bienestar_3), method=c("pearson", "kendall", "spearman"))

cor.test(as.numeric(base_85$eval_gob_1), as.numeric(base_85$region), method=c("pearson", "kendall", "spearman"))

# Gráfico de correlación

install.packages("corrplot")

## 5. Recodificar y crear una nueva variable 

## 5.1 Variable dummy 1 candidato--binaria
base_85$elec_pres_1.Dummy<-ifelse(base_85$elec_pres_1=="GABRIEL BORIC",1,0)
table(base_85$elec_pres_1.Dummy)

table_6 <- table(base_85$elec_pres_1.Dummy)
prop.table(table_6)

## 5.2 Recodificar variables de preferencias- Multiple

## Pregunta 14: Interes en la politica
table(base_85$interes_pol_1_b)
base_85$interes_pol_1_b_R = revalue(base_85$interes_pol_1_b, c("1"="2", "2"="2","3"="1", "4"="1", "5"="1", "8"="0","9"="0"))
table(base_85$interes_pol_1_b_R)

# Recodificar Confianza en la CC como dummy Para Trabajo 1
table(base_85$confianza_6_y)
base_85$confianza_6_y_Dummy = revalue(base_85$confianza_6_y, c("1"="1", "2"="1", "3"="1", "4"="0", "88"="0", "99"="0"))
table(base_85$confianza_6_y_Dummy)

## Recodificar Edad

base_85["Edad_Re2"] = cut(as.numeric(base_85$edad), c(0, 18, 30, 45, 55, Inf), c("0-17", "18-29", "30-44", "45-54", "55<"), include.lowest=TRUE)

table_8 <- table(base_85$Edad_Re2)

table(base_85$Edad_Re2)

table(base_85$edad)

base_85["Edad_Re3"] = cut(as.numeric(base_85$edad), c(18, 30, 45, 55, Inf), c("18-29", "30-44", "45-54", "55<"), include.lowest=TRUE)
table(base_85$Edad_Re3)

base_85["age_N"] <- as.numeric(base_85$edad)

table(base_85$age_N)

##as.numeric(as.character(f))

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad)) 

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad))

table(base_85$`age_N-2`)

base_85["Edad_Re6"] = cut(base_85$`age_N-2`, 
                          c(18, 30, 45, 55, Inf), c("18-29", "30-44", "45-54", "55<"), 
                          include.lowest=TRUE)


table(base_85$Edad_Re6)

## Recodificacion multiple para trabajo 1

##Recodificar votacion mayo
table(base_85$constitucion_5_a)
base_85$constitucion_5_a_Dummy = revalue(base_85$constitucion_5_a, c("1"="1", "2"="0", "88"="0", "99"="0"))
table(base_85$constitucion_5_a_Dummy)

##Recodificar Apoyo a protestas Multiple
table(base_85$ciudadania_29_a)
base_85$ciudadania_29_a_R = revalue(base_85$ciudadania_29_a, c("1"="1", "2"="1","3"="2", "4"="2", "5"="3", "88"="3", "99"="3"))
table(base_85$ciudadania_29_a_R)


##Recodificar pertenencia pueblo indigena dummy
table(base_85$info_enc_58)
base_85$info_enc_58_Dummy = revalue(base_85$info_enc_58, c("1"="1", "2"="0", "8"="0", "9"="0"))
table(base_85$info_enc_58_Dummy)

###Recodificacion para trabajo final
## Recodificar Apoyo a protestas Dummy 
table(base_85$ciudadania_29_a)
base_85$ciudadania_29_a_Dummy = revalue(base_85$ciudadania_29_a, c("1"="1", "2"="1","3"="1", "4"="1", "5"="0", "88"="0", "99"="0"))
table(base_85$ciudadania_29_a_Dummy)

##Recodificacion confianza a las personas a dummy 
table(base_85$confianza_8_a)
base_85$confianza_8_a_Dummy = revalue(base_85$confianza_8_a, c("1"="1", "2"="1", "3"="0", "4"="0", "88"="0"))
table(base_85$confianza_8_a_Dummy)

### Tablas de contingencia para trabajo 1  
#Ejercicio 1: Confianza en la CC dummy y haber votado en mayo
table(base_85$constitucion_5_a_Dummy)
sjt.xtab(base_85$confianza_6_y_Dummy, #filas
         base_85$constitucion_5_a_Dummy, #columnas
         show.col.prc = T, show.row.prc = T, file = "8.doc")

#Ejercicio 2: Confianza en la CC dummy y pertenencia indigena dummy
table(base_85$info_enc_58)
sjt.xtab(base_85$confianza_6_y_Dummy, #filas
         base_85$info_enc_58_Dummy, #columnas
         show.col.prc = T, show.row.prc = T, file = "9.doc")

##Ejercicio 3: Confianza en la CC dummy y apoyo en protestas recodificada
sjt.xtab(base_85$confianza_6_y_Dummy, #filas
         base_85$ciudadania_29_a_R, #columnas
         show.col.prc = T, show.row.prc = T, file = "11.doc")

##Ejercicio 4: Confianza en la CC dummy y sexo
sjt.xtab(base_85$confianza_6_y_Dummy, #filas
         base_85$sexo, #columnas
         show.col.prc = T, show.row.prc = T, file = "12.doc")

## 6. Análisis de correlación: Repaso 

library(corrplot)

table(base_85$eval_gob_1)
cor.test(as.numeric(base_85$eval_gob_1), as.numeric(base_85$region), 
         method=c("pearson"))
cor.test(as.numeric(base_85$eval_gob_1), as.numeric(base_85$region), 
         method = c("pearson"))

### Correlación para mi trabajo
#Correlacion Confianza CC y apoyo a protestas
cor.test(as.numeric(base_85$confianza_6_y_Dummy), as.numeric(base_85$ciudadania_29_a_R), 
         method=c("pearson"))

###Correlacion mi trabajo dummy CC y sexo
cor.test(as.numeric(base_85$confianza_6_y_Dummy), as.numeric(base_85$sexo), 
         method=c("pearson"))


### Confianza CC dummy  y pertenencia indigena dummy
cor.test(as.numeric(base_85$confianza_6_y_Dummy), as.numeric(base_85$info_enc_58_Dummy), 
         method=c("pearson"))

### Confianza CC dummy y votacion en mayo
cor.test(as.numeric(base_85$confianza_6_y_Dummy), as.numeric(base_85$constitucion_5_a), 
         method=c("pearson"))

## 7. Análisis de causalidad

### 7.1. Definir variables para regresión lineal ***** Ejemplo ya que 
### la variable dependiente tiene que ser continua

##### 7.1.1. Variable Dependiente: 
base_85$iden_pol_2 

# Variable Independiente  1: Edad
# Variable Independiente  2: Región
# Variable Independiente  3: GSE

##### 7.1.2. Variables numéricas

###### Edad: age_N-2

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad))

###### Region: region

base_85$region

###### GSE: Geupo socioeconómico

base_85["GSE-N"] <- as.numeric(as.character(base_85$gse))

###### 7.1.3. Definir X, Y 

Y <- cbind(base_85$iden_pol_2)
X1 <- cbind(base_85$`age_N-2`)
X <- cbind(base_85$`age_N-2`, base_85$region, base_85$`GSE-N`)

###### 7.1.4. Análisis de correlación

cor(X,Y)
cor(X1,Y)

###### 7.1.5. Gráfico de relación
plot(Y ~ X1, data = base_85)

## 8. Regresión Lineal SImple

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level = 0.95)
anova(olsreg1)

### 8.1. Graficando la regresión
abline(olsreg1)

### 8.2. Regresión 2

olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level = 0.95)
anova(olsreg2)

### 8.3. Graficando la regresión 2
abline(olsreg2)

### 9. Modelos de regresion usando stargazer

stargazer(olsreg1, olsreg2, title = "Resultados", 
          align =TRUE, out = "resultados1.txt")

### 10. Correr Regresion Logistica Binaria # El que se hace para trabajo final

### 10.1. Definir variables para regresión logistica

##### 10.1.1. Variable Dependiente: base_85$elec_pres_1.Dummy

table(base_85$elec_pres_1.Dummy)
# Variable Independiente  1: Edad
# Variable Independiente  2: Región
# Variable Independiente  3: GSE


m3 = glm(base_85$elec_pres_1.Dummy ~ X, data = base_85, family = binomial())
View(m3)
summary(m3)

# 11. Instalar funcion de bondad de ajuste
install.packages("rsq")

library(rsq)

# 12. Calcular bondad de ajuste
rsq(m3)

# 13. Tabular Regresion Logistica Binaria
stargazer(m3, title = "Modelo 3", align =TRUE, out = "resultados2.txt")

# 14. 1 modelo adicional m4

X4 <- cbind(base_85$`age_N-2`, base_85$region, base_85$`GSE-N`, 
            base_85$sexo)

m4 = glm(base_85$elec_pres_1.Dummy ~ X4,
         data = base_85,
         binomial())
summary(m4)

# 15. Tabular Regresion Logistica Binaria de m3 y m4
stargazer(m3, m4, title = "Modelos 3 y 4", align =TRUE, out = "resultados3.txt")

# 16. 2 modelos adicional m5 y m6 sin cbind()
m5 = glm(base_85$elec_pres_1.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1 + base_85$`GSE-N`,
         data = base_85,
         binomial())
summary(m5)

m6 = glm(base_85$elec_pres_1.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1  + base_85$`GSE-N` + base_85$sexo,
         data = base_85,
         binomial())
summary(m6)

# 17. Tabular Regresion Logistica Binaria de m5 y m6
stargazer(m5, m6, title = "Modelos 5 y 6", align =TRUE, out = "resultados4.txt")

## 18. Variable dummy religion_1 -- BINARIA
base_85$religion_1.Dummy<-ifelse(base_85$religion_1=="1",1,0)
table(base_85$religion_1.Dummy)

# 19. 2 modelos adicional m7 y m8 sin cbind()
m7 = glm(base_85$elec_pres_1.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1.Dummy + base_85$`GSE-N`,
         data = base_85,
         binomial())
summary(m7)

m8 = glm(base_85$elec_pres_1.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1.Dummy  + base_85$`GSE-N` + base_85$sexo,
         data = base_85,
         binomial())
summary(m8)

# 20. Tabular Regresion Logistica Binaria de m7 y m8
stargazer(m7, m8, title = "Modelos 7 y 8", align =TRUE, 
          out = "resultados5.txt")


###Regresion para mi trabajo final
##### Variable Dependiente: 
base_85$confianza_6_y_Dummy

table(base_85$confianza_6_y_Dummy)

# Variable Independiente  1: Participacion politica R constitucion_5_a:
# Variable Independiente  2: Apoyo a las marchas como protesta legitima R ciudadania_29_a:
# Variable Independiente  3: Sexo sexo no r
# Variable Independiente  4: Pertenencia indigena recodificada R info_enc_58

##mio m9 Todas mis variables

m9 = glm(base_85$confianza_6_y_Dummy ~ base_85$constitucion_5_a_Dummy + 
           base_85$ciudadania_29_a_Dummy + base_85$sexo + base_85$info_enc_58_Dummy , 
         data = base_85, 
         binomial())
summary(m9)


##mio m10 Todas mis variables mas urbano rural 
m10 = glm(base_85$confianza_6_y_Dummy ~ base_85$constitucion_5_a_Dummy + 
            base_85$ciudadania_29_a_Dummy + base_85$sexo + base_85$info_enc_58_Dummy
          +base_85$zona_u_r, 
          data = base_85, 
          binomial())

## mio m11 Todas mis variables mas confianza a las personas dummy
m11 = glm(base_85$confianza_6_y_Dummy ~ base_85$constitucion_5_a_Dummy + 
            base_85$ciudadania_29_a_Dummy + base_85$sexo + base_85$info_enc_58_Dummy 
          + base_85$confianza_8_a_Dummy, 
          data = base_85, 
          binomial())
summary(m11)


# Tabular Regresion Logistica Binaria de m9 y m10
stargazer(m9, m10, title = "Modelos 9 y 10", align =TRUE, 
          out = "resultados6.txt")

# Tabular Regresion Logistica Binaria de m9 y m10 y m11
stargazer(m9, m10, m11, title = "Modelos 9 y 10 y 11", align =TRUE, 
          out = "resultados7.txt")
