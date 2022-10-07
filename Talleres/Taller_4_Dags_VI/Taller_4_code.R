############################################################################################################################/
##################### Taller 4: DAGS Y Variables Instrumentales #####################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Revisión conceptual de variables instrumentales
# 2. Aplicar análisis de variables instrumentales en R

############################################################################################################################/
# 0. Ajustes iniciales  ----------------------------------------------------------------
############################################################################################################################/

# Ajustes de arranque 
options(scipen=999) # Desactiva la notación científica
options(max.print = 99999999) # Max print de la consola
rm(list=(ls()))   # limpia el enviroment (dispongo de m?s memoria temporal)

# Vamos a cargar algunas librer?as que vamos a utilizar
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

install_load(c("rio", "dplyr", "ggplot2", "stringr", "tidyr", "kableExtra", 
               "plotly", "janitor", "texreg", "Publish", "broom", "ggpubr", 
               "car", "lmtest", "estimatr", "texreg", "AER", "lfe", "SteinIV", "sandwich"))

sessionInfo()

############################################################################################################################/
# Primera estimación IV ----------------------------------------------------------------
############################################################################################################################/

# En esta primera aproximación vamos a revisar algunos resultados de: Card, David. 1995. “Aspects of Labour Economics: Essays in Honour of John Vanderkamp.” In. University of Toronto Press.

# El autor está interesado en los retornos de la escolarización sobre los salarios:

# Yi=B0 + BSi + BXi + ei

# Propone una estrategia de variables instrumentales mediante la cual instrumentará la escolarización con la variable ficticia de la universidad en el condado.

# Los datos provienen de la cohorte de hombres jóvenes de NLS de la Encuesta Nacional Longitudinal. 
# Estos datos comenzaron en 1966 con 5.525 hombres de entre 14 y 24 años y continuaron su seguimiento hasta 1981.

# Simplemente estimaremos un modelo 2SLS con OLS.

# Pueden revisar con mayor detalle aquí: https://mixtape.scunning.com/instrumental-variables.html#college-in-the-county

# 1. ¿Cree que este instrumento cumple con el supuesto de independencia?
 
# 2. ¿Cree que cumple con el supuesto de exclusión?

# 3. ¿Cree que cumple con el supuesto de first stage?

# 4. ¿Cree que cumple con el supuesto de monotonicity?

# 5. ¿Quienes son los compliers en este caso?

# 6. ¿Quienes son los never takers?

# 7. ¿Quienes son los always takers?

# 8. ¿Qué estima el LATE?

# 9. ¿Que nos dicen los resultados de OLS?

# Generamos una función para cargar los datos
read_data <- function(df) {
  full_path <- paste0("https://raw.github.com/scunning1975/mixtape/master/", df)
  return(haven::read_dta(full_path))
}

#Cargamos la data
card <- read_data("card.dta")

#OLS
ols_reg <- lm(lwage ~ educ + exper + black + south + married + smsa, 
              data = card)

summary(ols_reg)

# 11. ¿Cuál es el first stage y el efecto del tratamiento? Calcule a través de 2SLS.

paso1 <- lm(educ ~ exper + black + south + married + smsa + nearc4, 
            data = card, na.action = na.exclude)

card$pred <- predict(paso1)

paso2 <- lm(lwage ~ pred + exper + black + south + married + smsa, 
            data = card)

summary(paso2)

# 10. ¿Qué nos dicen los resultados cuando utilizamos 2SLS?

iv_reg = ivreg(lwage ~ educ + exper + black + south + married + smsa |
                nearc4 + exper + black + south + married + smsa, 
               data = card)

summary(iv_reg)

# 12. ¿Habría esperado que los resultados de 2SLS fueran mayores a los de OLS?


############################################################################################################################/
# Ejemplo aplicado ----------------------------------------------------------------
############################################################################################################################/

# Experimento sobre mobilizaci ́on de votantes en New Haven para las elecciones generales de 1998 (Gerber y Green 2000).

# Muestra de 7.090 individuos que viven en hogares de un solo votante.

# Tratamiento
# Grupo de tratamiento: visita de canvassers que señalaron la importancia de ir votar. Visitas fueron hechas Sabados y Domingos durante cuatro semanas antes de la elecci ́on.
# Grupo de control: no recibe visita.
# Variable dependiente: si votó o no en la elección.

data1 <- import("Data/Boxes5_4_5_5.dta")

#ITT: En el grupo de tratado que recibieron el tratamiento y otros que no recibieron el tratamiento. Estoy hablando de la posibilidad
#de que pudo recibir el tratamiento. 

# Vea que han cargado bien los datos
colnames(data1)
names(data1)
dim(data1)

# Seleccionar hogares unipersonales que son contactados puerta a puerta (canvass)
sel <-  data1$onetreat==1 & data1$mailings==0 & data1$phongotv==0 & data1$persons==1

# Verificar número de observaciones
table(sel) #Vamos a trabajar con los hogares unipersonales. 
data2 <- data1[sel,]  #Se puede ocupar dplyer

# Definir variables
v98      <- data2$v98 #VD, voto o no voto
persngrp <- data2$persngrp #tratamiento
cntany   <- data2$cntany #asignación

#Estimación de un modelo de probabilidad lineal. 
#--------------------------Estimaciones---------------------------------------#

# Box 5.4: ITT: efecto de la asignación
# -----------/

# SE no robustos
coef(summary(lm(v98 ~ persngrp))) #Los tipos que son asignados al tratamiento tienen un efecto de 0.38
# SE robustos
itt_fit <- lm(v98 ~ persngrp)
coeftest(itt_fit,vcovHC(itt_fit))
coeftest(itt_fit,vcovHC(itt_fit, typt="HC2"))

# Box 5.5: ITT_D
# --------------/

# SE no robustos
coef(summary(lm(cntany ~ persngrp)))
# Se robustos: 27% de cumplidores
itt_d_fit <- lm(cntany ~ persngrp)
coeftest(itt_d_fit,vcovHC(itt_d_fit)) 

# Box 5.6: CACE
# ------------/

# SE no robustos
coef(summary(ivreg(v98 ~ cntany,~persngrp)))#coeficiente es el CACE

# SE robustos por homocedasticidad
cace_fit <- ivreg(v98 ~ cntany,~persngrp) #ivreg:paquete AER. Dos pasos de la variable instrumental.  
coeftest(cace_fit,vcovHC(cace_fit))#Escarba en las subpoblaciones 

############################################################################################################################/
# Aplicando VI para la tarea 2 ----------------------------------------------------------------
############################################################################################################################/
# Importamos la BBDD
# data <- haven::read_dta("ang_ev_1980.dta")
data_iv <- import("Data/ang_ev_1980.dta")
colnames(data_iv)
str(data_iv)

############################################################################################################################/
# First Stage (Tabla 5 columna 1) ----------------------------------------------------------------
############################################################################################################################/
#*
table(data_iv$samesex)
table(data_iv$twins_2)
table(data_iv$morekids)
table(data_iv$kidcount)

## Ejemplo de base IV: 
## Estimemos el modelo solicitado 
m11 <- lm(morekids ~ samesex, data=data_iv)
se_m11 <- sqrt(diag(vcov(m11)))
coef(summary(m11))[, "Std. Error"]

data_iv$predicciones <- m11$fitted.values
#data_iv$residuos <- m11$residuals # No es necesario por ahora.
mod_iv <- lm(mom_worked ~ predicciones, data=data_iv)
screenreg(mod_iv, digits = 4)

# Tabla 5 columna 1, fila 1 y 2
m11 <- lm(morekids ~ samesex, data=data_iv)
m12 <- lm(kidcount ~ samesex, data=data_iv)
screenreg(l=list(m11,m12), include.ci = FALSE,  digits = 4)

############################################################################################################################/
# Modelo con variable instrumental (Tabla 5 columna 2, fila 3) ----------------------------------------------------------------
############################################################################################################################/
# Dummy tiene 2 o más hijos
m23 <- ivreg(mom_worked ~ morekids | samesex, data = data_iv)
m23b <- estimatr::iv_robust(mom_worked ~ morekids | samesex, data = data_iv)

screenreg(m23, include.ci = FALSE,  digits = 4)
screenreg(m23b, include.ci = FALSE,  digits = 4)
screenreg(mod_iv, include.ci = FALSE,  digits = 4)

screenreg(l=list(mod_iv,m23,m23b), include.ci = FALSE,  digits = 4)

# Tabla 5 columna 2 filas 3 
m23 <- ivreg(mom_worked ~ morekids | samesex, data = data_iv)

screenreg(l=list(m23), include.ci = FALSE,  digits = 4)

############################################################################################################################/
# Modelo con variable instrumental (Tabla 5 columna 7, filas 1-2) ----------------------------------------------------------------
############################################################################################################################/

m71 <- lm(morekids ~ twins_2, data=data_iv)
m72 <- lm(kidcount ~ twins_2, data=data_iv)

screenreg(l=list(m71, m72), include.ci = FALSE,  digits = 4)

############################################################################################################################/
# Modelo con variable instrumental (Tabla 5 columna 8, filas 3) ----------------------------------------------------------------
############################################################################################################################/

m83 <- ivreg(mom_worked ~ morekids | twins_2, data = data_iv)

screenreg(l=list(m83), include.ci = FALSE,  digits = 4)

############################################################################################################################/
# Compliance ----------------------------------------------------------------
############################################################################################################################/

# Always taker
prop.table(table(data_iv$morekids)) # 1

# Compliers
prop.table(table(data_iv$samesex, data_iv$morekids), margin = 1)
#prop.table(table(data_iv$samesex, data_iv$morekids))

# Never takers
prop.table(table(data_iv$morekids)) # 0

# Los 1 mezcla a los always taker + compliers 
summary(lm(morekids ~ samesex, data=data_iv))

############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/


