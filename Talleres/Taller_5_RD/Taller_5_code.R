############################################################################################################################/
##################### Taller 5: Regresión Discontinua #####################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Aplicar análisis de regresión discontinua en R
# 2. Uso y aplicaciones de DAGS 

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
              "texreg", "Publish", "broom", "ggpubr", "estimatr", "texreg", 
              "AER", "lfe", "SteinIV", "sandwich", 
              "ggdag", "dagitty",
              "rddensity", "lpdensity", "rdrobust"))

sessionInfo()

############################################################################################################################/
# I. Regresión Discontinua: Aplicación 1 (SHARP) ----------------------------------------------------------------
############################################################################################################################/

# MLDA: reducción de la edad de venta de alcohol de 21 años a 18 años en USA. 
# El objetivo del ejercicio es estimar el efecto de MLDA de 21 años en causas de muertes.

# MLDA genera un experimento natural que puede ser usado para estudiar el efecto del acceso legal al alcohol en tasas de mortalidad.

# VDependiente es muertes por 100 mil personas por año; X es edad en meses. Cada punto es la tasa de mortalidad en un intervalo mensual.

# 1. ¿Cuál es el puntaje?

# 2. ¿Cuál es el punto de corte?

# 3. ¿Tratamiento? ¿Outcome?

# Veamos un primer ejemplo:

load("data/mlda.rda") # Cargamos la data 

names(mlda)
mlda$agecell

## 1. Running variable ----

mlda <- mlda %>% mutate(age = agecell - 21, #running variable
                        over21 = as.integer(agecell >= 21)) #p=el término de asignación. Tipo de variable categórica pariente de factor (dummy)

View(mlda)

# Running variable: age
mlda$agecell
mlda$age #distancia respecto de 0

## 2. Efecto del tratamiento ----

# Tratamiento: over21
mlda$agecell
mlda$over21
#0=control; 1=tratado

# Tratamiento: all
mlda$all	#Tasa de mortalidad, dependiente. Dplyer no puede trabajar con missing.

# Grafico para variable resultado "all"
a1 <- mlda %>% 
  select(agecell, over21, all) %>%
  gather(response, value, -agecell, -over21, na.rm = TRUE) 

a1

# Visualicemos
ggplot(a1,aes(x = agecell, y = value)) +
  geom_point() + #gráfico de puntos 
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ poly(x, 2)) +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") + 
  labs(y = "Mortality rate (per 100,000)", x = "Age")	+
  theme_light()

varlist <- c("all" = "All Causes",
             "mva" = "Motor Vehicle Accidents",
             "internal" = "Internal Causes")	

a1 <- mlda %>% 
  select(agecell, over21, all, mva, internal) %>%
  gather(response, value, -agecell, -over21, na.rm = TRUE) 

a1

a1 %>% ggplot(aes(x = agecell, y = value)) +
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ poly(x, 2)) +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") +
  facet_grid(response ~ ., scales = "free_y") + # Crea panel con 3 VD
  labs(y = "Mortality rate (per 100,000)", x = "Age")		+
  theme_light()

## 3. Modelo ----

# Edad 19-22, lineal
m1 <- lm(all ~ age + over21, data = mlda)
m1b<-coeftest(m1, vcov.= vcovHC(m1, "HC2"))
m1b

# Edad 19-22, cuadratico (2=efecto cuadrático)
m2 <- lm(all ~ poly(age, 2, raw = TRUE) * over21, data = mlda) #Interacciones para capturar patrones no lineales. 
m2b<-coeftest(m2, vcov.= vcovHC(m2, "HC2"))
m2b

# Otra manera más directa
summary(estimatr::lm_robust(all ~ poly(age, 2, raw = TRUE) * over21, data = mlda, se_type = "HC2"))

# Edad 20-21, lineal
m3 <- lm(all ~ age + over21,
         data = filter(mlda, agecell >= 20, agecell <= 22)) #ancho banda. 
m3b<-coeftest(m3, vcov.= vcovHC(m3, "HC2"))
m3b

# Edad 20-21, cuadratico
m4 <- lm(all ~ poly(age, 2, raw = TRUE) * over21,
         data = filter(mlda, agecell >= 20, agecell <= 22))
m4b<-coeftest(m4, vcov.= vcovHC(m4, "HC2"))
m4b

# Reportamos nuestros modelos
screenreg(l=list(m1b,m2b,m3b,m4b), single.row = TRUE, stars = c(0.01, 0.05))		 

############################################################################################################################/
# II. Regresión Discontinua: Aplicación 2 (FUZZY) ----------------------------------------------------------------
############################################################################################################################/

# Veamos un segundo ejemplo con el efecto del CAE sobre outcomes educacionales
data <- import("data/data.dta")

## 1. Estandarizamos la running variable ----
data <- data %>% 
  mutate(runvar=promlm-475)

## 2. Analice si hay evidencia de manipulación utilizanto rddensity ----
summary(rddensity(X = data$runvar))

rdplotdensity(rddensity(X = data$runvar), X = data$runvar,
              lcol = c("blue", "red"), 
              alpha = 0.05,
              #hist=FALSE,
              histFillCol = 1,
              title = "Density running variable",
              xlabel = "Running Variable",
              ylabel = "Densidad",
              plotRange = c(-100, 100))

## 3. Elija el bandwidth óptimo ----
summary(rdbwselect(y=data$mat_cruch, x=data$runvar))

## 4. Muestre un gráfico con el efecto del programa ----

?rdplot # Ver argumentos del RD plot.
# El argumento "p=4" Nos ayuda a cambiar el orden del polinomio.
# "kernel=..." Nos ayuda a definir el kernel utilizado.
rdplot(y=data$mat_cruch, x=data$runvar, 
       title = "RD del Efecto del programa",
       x.label="Running Variable",
       y.label="Probabilidad de ingresar en la Universidad CRUCH",
       y.lim=c(0,1),
       x.lim=c(-100,100),
       col.dots=c("steelblue"))

# Ajustemos con el bandwith óptimo
rdplot(y=data$mat_cruch, x=data$runvar, 
       title = "RD del Efecto del programa",
       x.label="Running Variable",
       y.label="Probabilidad de ingresar a la Universidad CRUCH",
       y.lim=c(0,1),
       x.lim=c(-57,57),
       col.dots=c("steelblue"))

## 5. Estima una regresión con el efecto del programa sobre la probabilidad de ingresar a la U ----

# Generamos variables
data <- data %>%  
  mutate(above=if_else(runvar>=0, 1, 0),  # "Tratamiento"
         instrunvar=runvar*above)        # Instrumento

# Filtramos las observaciones según bandwidth óptimo
data_trimmed <- data %>% filter(abs(runvar)<=57)
rd <- lm(mat_cruch ~ above + runvar + instrunvar, data=data_trimmed)
summary(rd)

## 6. ¿Hay balance en la educación de la madre? ----
bal <- lm(mother_tertiary ~ above + runvar + instrunvar, data=data_trimmed)
summary(bal)

############################################################################################################################/
# Extra: DAGS (Directed Acyclic Graphs) ----------------------------------------------------------------
############################################################################################################################/

# Puede consultar aquí: https://evalf20.classes.andrewheiss.com/example/dags/

# Veamos un ejemplo con el uso de variables instrumentales
# Tomemos de referencia el estudio de CARD sobre educación y salarios 

# Generamos una función para cargar los datos
read_data <- function(df) {
    full_path <- paste0("https://raw.github.com/scunning1975/mixtape/master/", df)
    return(haven::read_dta(full_path))
}

#Cargamos la data
card <- read_data("card.dta")

# Representemos gráficamente el modelo 
ingresos_dag <- dagify(
    lwage ~ educ + exper + black,
    educ ~ exper + black, 
    exposure = "educ",
    outcome = "lwage",
    coords = list(educ = c(educ = 1, expr = 2, black = 2, lwage = 3),
                  lwage = c(educ = 2, expr = 1, black = 3, lwage = 2)),
    labels = c(lwage="Ingresos", educ="Educación", 
               exper="Experiencia Laboral", black="Afroamericana")
)

ggdag_status(ingresos_dag, use_labels = "label", text = FALSE) + 
    guides(fill = FALSE, color = FALSE) +  # Disable the legend
    theme_dag()

ggdag_adjustment_set(ingresos_dag, shadow = TRUE) +
    theme_dag()

# Agreguemos una variable instrumental (distancia de los estudiantes respecto a la universidad)
ingresos_dag <- dagify(
    lwage ~ educ + exper + black,
    educ ~ exper + black, 
    educ ~ nearc4, 
    exposure = "educ",
    outcome = "lwage",
    coords = list(educ = c(educ = 1, expr = 2, black = 2, lwage = 3, nearc4=-1),
                  lwage = c(educ = 2, expr = 1, black = 3, lwage = 2, nearc4=0)),
    labels = c(lwage="Ingresos", educ="Educación", 
               exper="Experiencia Laboral", black="Afroamericana",
               nearc4="Distancia Universidad")
)

ggdag_status(ingresos_dag, use_labels = "label", text = FALSE) + 
    guides(fill = FALSE, color = FALSE) +  # Disable the legend
    theme_dag()

ggdag_adjustment_set(ingresos_dag, shadow = TRUE) +
    theme_dag()

#OLS
ols_reg <- lm(lwage ~ educ + exper + black + south + married + smsa, 
              data = card)

summary(ols_reg)

#2SLS
iv_reg = ivreg(lwage ~ educ + exper + black + south + married + smsa |
                   nearc4 + exper + black + south + married + smsa, 
               data = card)

summary(iv_reg)

screenreg(l=list(ols_reg, iv_reg))

############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/


