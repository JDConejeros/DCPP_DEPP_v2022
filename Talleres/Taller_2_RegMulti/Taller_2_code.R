############################################################################################################################/
##################### Taller 2: Regresión Múltiple ###############################################################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Introducción a las herramientas de Rmarkdown
# 2. Evaluación del sesgo por variable omitida
# 3. Idea general del control estadístico y balance por covariables
# 4. Aplicación de modelos de regresión lineal múltiple
# 5. Supuestos de OLS BLUE

#**************************************************************************************************************************/
# 0. Ajustes iniciales  ----------------------------------------------------------------
#**************************************************************************************************************************/

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
               "car", "lmtest"))

sessionInfo()

#**************************************************************************************************************************/
# 1. Introducción a Rmarkdown ----------------------------------------------------------------
#**************************************************************************************************************************/

## a. Utilice el siguiente código para construir un documento en Rmarkdown:

#Crear una tabla con valores 
tabla <- data.frame(pais = c("Inglaterra", "Alemania", "Portugal", "Dinamarca", "Chile"), 
                    gto_social = c(16, 29.7, 21.6, 34.3,9.8),                        
                    votacion_izq = c(20, 37, 34.4, 41.9,20))    
#Ver la estructura de la tabla
str(tabla)
tabla #Esta tabla no es de calidad

kable(tabla, booktabs =TRUE,                  #Estilo de la tabla
      align = c("ccc"),                       #Alineación de columnas
      linesep = "",                           #Sin línea entre columnas
      caption = "Tabla de calidad",           #Título
      col.names =c("País", "Gasto Social (%)", 
                   "Votación de izquierda (%)")) %>% #Nombre de las columnas 
  kable_styling(latex_options =c("hold_position")
                , full_width = F) %>%         #Posición de tabla
  kableExtra::footnote(general = "Elaboración propia.", footnote_as_chunk = T, 
                       general_title="Fuente:", fixed_small_size=T) #Nota al pie de la tabla

# Vamos a estimar un modelo simple:
modelo <- lm(gto_social ~ votacion_izq, data=tabla) #Estimar
modelo

# Tabulamos los resultados del modelo
mod <- tidy(modelo)
mod$term <- c("Intercepto", "Votación por la Izquierda (%)")
kable(mod, booktabs =TRUE, digits=3, align = c("ccccc"), caption = "Modelo OLS para Gasto Social",       
      col.names=c(" ", "Beta", "Error estádar", "Valor t", "Valor p")) %>% 
  kable_styling(latex_options =c("hold_position"), full_width = F) %>%  
  kableExtra::footnote(general = "Elaboración propia.", footnote_as_chunk = T, 
                       general_title="Fuente:", fixed_small_size=T)

texreg(l=list(modelo),                               #Modelo a utilizar 
       caption="Modelo OLS para Gasto Social",       #Título
       custom.coef.names=c("Intercepto","Votación por la Izquierda (%)"), #Nombre de variables
       float.pos="h" , digits = 3,                  #Posición y dígitos
       single.row = FALSE,                          #Sin línea de fila
       fsingle.row = T,
       caption.above = TRUE,                        #Título sobre la tabla
       include.ci = FALSE,                          #Mostrar el intervalo
       stars = c(0.01, 0.05, 0.1))                  #Mostrar el nivel de significancia

htmlreg(l=list(modelo),                               #Modelo a utilizar 
       caption="Modelo OLS para Gasto Social",       #Título
       custom.coef.names=c("Intercepto","Votación por la Izquierda (%)"), #Nombre de variables
       float.pos="h" , digits = 3,                  #Posición y dígitos
       single.row = FALSE,                          #Sin línea de fila
       fsingle.row = T,
       caption.above = TRUE,                        #Título sobre la tabla
       include.ci = FALSE,                          #Mostrar el intervalo
       stars = c(0.01, 0.05, 0.1))                  #Mostrar el nivel de significancia

screenreg(l=list(modelo),                               #Modelo a utilizar 
       caption="Modelo OLS para Gasto Social",       #Título
       custom.coef.names=c("Intercepto","Votación por la Izquierda (%)"), #Nombre de variables
       float.pos="h" , digits = 3,                  #Posición y dígitos
       single.row = FALSE,                          #Sin línea de fila
       fsingle.row = T,
       caption.above = TRUE,                        #Título sobre la tabla
       include.ci = FALSE,                          #Mostrar el intervalo
       stars = c(0.01, 0.05, 0.1))                  #Mostrar el nivel de significancia

# Presentar un gráfico con la regresión 
g1 <- ggplot(tabla, aes(y=gto_social, x=votacion_izq)) +
  geom_smooth(method = "lm", colour="black", se=T, formula=y ~ x) +
  geom_point() +
  geom_text(aes(label=pais),hjust=0.5, vjust=-1.2, size=2.5) +
  labs(y = "Gasto Social (%)", x = "Votación por la Izquierda (%)", 
       title="", caption = "Fuente: Elaboración propia. N=4 observaciones") +
  theme_light() +
  theme(axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8))
g1  

#**************************************************************************************************************************/
# 2. Sesgo por variable omitida ----------------------------------------------------------------
#**************************************************************************************************************************/

# En la encuesta CASEN 2017 se pregunta si los/as niños/as entre 0 y 6 años recibieron alimentos gratuitos 
# del consultorio u hospital en los últimos 3 meses. Además, se pregunta a las familias que evalúen 
# la salud de los/as niños/as en una escala que va desde el valor 1 (peor) al valor 7 (mejor)

casen <- import("Data/casen_2017_reducida.dta")
glimpse(casen); names(casen)

# s2a1: Recibe alimentos del consultorio u hospital en los últimos 3 meses 
# edad: Años de edad 
# s13: Evaluación de salud
# qaut: Quintil autónomo nacional

# Algunos procesamientos de la base de datos
table(casen$s2a1, useNA = "ifany")    
table(casen$edad, useNA = "ifany")   
table(casen$s13, useNA = "ifany")   
table(casen$qaut, useNA = "ifany")  

# Generamos las variables nuevas a partir de las originales
casen <- casen %>% filter(edad <= 6) %>%
  mutate(alimento=if_else(s2a1<=6,1, if_else(s2a1==7, 0, NA_real_)),
         salud=if_else(s13==9, NA_real_, s13),
         qaut=factor(qaut, labels = c("I", "II", "III", "IV", "V")),
         q1=if_else(qaut=="I", 1, 0),
         q5=if_else(qaut=="V", 1, 0))

# Por simplicidad borraremos los missing
casen <- casen %>% drop_na()
glimpse(casen); names(casen)

# Primer modelo OLS
m1 <- lm(salud ~ alimento, data=casen)
screenreg(l=list(m1), caption="Modelo OLS para autoreporte de salud",
       custom.coef.names=c("Intercepto","Programa de Alimentos"), float.pos="h",
       digits = 3, single.row = FALSE, fsingle.row = T, caption.above = TRUE,
       include.ci = FALSE, stars = c(0.01, 0.05, 0.1))

# ¿Habrá autoselección? Distribución por nivel de ingreso
tabla <- casen %>% group_by(alimento, qaut) %>%
  summarise(n=n()) %>% 
  mutate(prop=round(n/sum(n)*100, 2),
         prop=paste0(prop, "%")) %>% 
  select(qaut, alimento, prop) %>% 
  spread(alimento, prop)

# Presentamos la tabla
# Podríamos crear dummies y aplicar test de diferencias para proporciones r. 

kable(tabla, booktabs=TRUE, align=c("ccc"), linesep="", caption="Diferencias entre SES",
      col.names=c("Quintil de ingreso", "No recibe alimentos (Di=0)", "Recibe alimentos (Di=1)")) %>%
  kable_styling(latex_options=c("hold_position"), full_width = F) %>% 
  kableExtra::footnote(general="Elaboración propia. CASEN 2017 (datos no ponderados).",
                       footnote_as_chunk=T, general_title="Fuente:", fixed_small_size=T)

# El beta de regresión estará sesgado si observamos dos condiciones:

## a. X2i es relevante para Yi: 
summary(lm(salud~factor(q5), data=casen))
round(summary(lm(salud ~ factor(q1), data=casen))[["coefficients"]], 3) # Signo: (-)
round(summary(lm(salud ~ factor(q5), data=casen))[["coefficients"]], 3) # Signo: (+)

## b. X2i está asociado con X1i: Asosciación significativa entre SES y recibir el beneficio
summary(lm(alimento~factor(q1), data=casen))
round(summary(lm(alimento ~ factor(q1), data=casen))[["coefficients"]], 3) # Signo: (+)
round(summary(lm(alimento ~ factor(q5), data=casen))[["coefficients"]], 3) # Signo: (-)

## Si ambas condiciones se cumplen beta1 estimado será sesgado respecto al valor valor verdadero a nivel poblacional.

## ¿Qué ocurre sino podemos observar X2? ¿Y el signo de b2?

## ¿Cuál sería la dirección del sesgo?

# |               | C(X1,X2)>0     | C(X1,X2)<0       |
# |:-------------:|:--------------:|:----------------:|
# | B2 > 0        |  +             | -                | 
# | B2 < 0        |  -             | +                |       

# Si omitimos el q1: (-) Subestimado b1

# Si omitimos el q5: (-) Subestimado b1

## ¿Cuál sería la magnitud del sesgo? C(X1,X2)*B2

# Si omitimos la variable que representa el quintil más pobre:
b2 <- round(lm(salud ~ factor(q1), data=casen)[["coefficients"]][["factor(q1)1"]], 3) 
x1_x2 <- round(lm(alimento ~ factor(q1), data=casen)[["coefficients"]][["factor(q1)1"]], 3) 

# ¿Qué ocurre si alguna de estas estimaciones es 0?
b2 
x1_x2

sesgo=x1_x2*b2

sesgo

## Calcule y discuta signo y dirección del sesgo para el q5.
summary(lm(salud ~ factor(q5), data=casen))
b2 <- round(lm(salud ~ factor(q5), data=casen)[["coefficients"]][["factor(q5)1"]], 3) 

summary(lm(alimento ~ factor(q5), data=casen))
lm(alimento ~ factor(q5), data=casen)

x1_x2 <- round(lm(alimento ~ factor(q5), data=casen)[["coefficients"]][["factor(q5)1"]], 3) 

sesgo=x1_x2*b2

sesgo

## ¿Qué otras variables no observables se podrían considerar en estos datos? 

#**************************************************************************************************************************/
# 3. Control estadístico ----------------------------------------------------------------
#**************************************************************************************************************************/

# ¿Cómo analizar la asociación/efecto entre X e Y sin la influencia de X2?
# Bloquear el camino entre X e Y que pasa por X2. En otras palabras, dejar constante X2 o controlar por X2.
# ¿Hay algún modo de limpiar la asociación entre X2 e Y para quedarnos con la parte de Y que no es explicada por X2?

# Residualización:

# a. Estime y reporte un primer modelo con la regresión bivariada:
m1 <- lm(alimento ~ factor(q1), data=casen)
summary(m1)

# b. Obtenga las predicciones de su primer modelo y genere una variable nueva en su base de datos con los residuos.

# Obtenemos las predicciones del modelo 
predict(m1) 
casen$predichos <- predict(m1)
head(casen)

# Generamos una variable con los residuos del modelo
casen$residuos <- casen$alimento - casen$predichos 
head(casen)

casen$residuos2 <- residuals(m1)
head(casen)
# Estime y reporte un segundo modelo de regresión bivariada:
m2 <- lm(salud ~ residuos, data=casen)
summary(m2)

# Podemos controlar dentro de la misma regresión
m3 <- lm(salud ~ factor(alimento) + factor(q1), data=casen)
summary(m3)

# Podemos incluir más controles estadísticos 
m0 <- lm(salud ~ factor(alimento), data=casen)
m1 <- lm(salud ~ factor(alimento) + factor(q1), data=casen)
m2 <- lm(salud ~ factor(alimento) + factor(q1) + factor(q5), data=casen)

# Reportemos todos los modelos juntos 
screenreg(l=list(m0, m1, m2), caption="Regresiones OLS", float.pos="h" , digits = 5, 
          caption.above = TRUE,  include.ci = FALSE, stars = c(0.01, 0.05, 0.1), fsingle.row = T)

############################################################################################################################/
# 4. Regresión Lineal Múltiple ----------------------------------------------------------------
############################################################################################################################/

# Aplicaremos todo lo visto a una forma más generalizada de la regresión múltiple con k variables.
# Recuerde que el objetivo de OLS es siempre minimizar las distancia de los residuos.
# Ejemplo adaptado de: Khandker, S. R., Koolwal, G. B., & Samad, H. A. (2009). Handbook on impact evaluation: quantitative methods and practices. World Bank Publications.

# Vamos a evaluar los impactos de un programa de microcréditos sobre los gastos per-cápita de los hogares. 

exp <- import("Data/hh_98.dta")
dim(exp); glimpse(exp); names(exp)

# Nivel de hogar
table(exp$dmmfd)
table(exp$dfmfd)

hist(exp$exptot)

# Ajuste de variables
exp <- exp %>% 
  mutate(lexptot = log(1 + exptot), # Logaritmo del total de gastos por hogar/año
         lnland = log((1 + hhland/100)))  # Acres de tierra antes de unirse al programa

hist(exp$lexptot)

# Para evaluar potenciales variables omitidas, podemos realizar un análisis de balacen por covariables observadas
#Diferencia de Medias
#Podríamos realizar 1 por 1 los test 
#dfmfd:  HH has female microcredit participant: 1=Y, 0=N

t.test(exp$sexhead ~ factor(exp$dfmfd), alternative = "two.sided", conf.level = 0.95)  # Sexo jefe de hogar
t.test(exp$agehead ~ factor(exp$dfmfd))     # Edad jefe de hogar
t.test(exp$educhead ~ factor(exp$dfmfd))    # Educación jefe de hogar
t.test(exp$lnland ~ factor(exp$dfmfd))       # Acres de tierra antes de unirse al programa
t.test(exp$vaccess ~ factor(exp$dfmfd))  # Accesibilidad por carretera
t.test(exp$pcirr ~ factor(exp$dfmfd)) # Proporción de riego
t.test(exp$rice ~ factor(exp$dfmfd))      # Precio del arroz por kilo
t.test(exp$wheat ~ factor(exp$dfmfd))         # Precio del trigo por kilo
t.test(exp$milk ~ factor(exp$dfmfd))         # Precio de la leche por litro
t.test(exp$oil ~ factor(exp$dfmfd))         # Precio del aceite comestible por kilo
t.test(exp$egg ~ factor(exp$dfmfd))         # Precio del huevo por 4 unidades

#O podríamos realizar un proceso iterativo
balance <- c("sexhead", "agehead", "educhead", "lnland", "vaccess",
             "pcirr", "rice", "wheat", "milk", "oil", "egg")

#Generamos los vectores
control <- c()
ci_ic   <- c()
ci_sc   <- c()
tratado <- c()
ci_it   <- c()
ci_st   <- c()
dif     <- c()
pvalue  <- c()
#Completamos la información
for (i in balance){
  control[i]  <- round(mean(exp[[i]][exp$dfmfd==0], na.rm = T),3)
  ci_ic[i]    <- round(ci.mean(exp[[i]][exp$dfmfd==0])[["lower"]],3) 
  ci_sc[i]    <- round(ci.mean(exp[[i]][exp$dfmfd==0])[["upper"]],3) 
  tratado[i]  <- round(mean(exp[[i]][exp$dfmfd==1], na.rm = T),3)
  ci_it[i]    <- round(ci.mean(exp[[i]][exp$dfmfd==1])[["lower"]],3) 
  ci_st[i]    <- round(ci.mean(exp[[i]][exp$dfmfd==1])[["upper"]],3) 
  dif[i]      <- tratado[i] - control[i]
  pvalue[i]   <- round(t.test(exp[[i]] ~ factor(exp$dfmfd))$p.value, 3) 
}

test <- as.data.frame(cbind(balance, control, ci_ic, ci_sc, tratado, ci_it, ci_st, dif, pvalue))
test2 <- test  %>%  dplyr::select("balance", "control", "tratado", "dif", "pvalue")
test2

# Diferencia de promedios
g1 <- test2 %>%  ggplot(aes(y=factor(balance), x=as.numeric(dif))) +
  geom_segment(aes(x=0, xend=as.numeric(dif), 
                   y=factor(balance), yend=factor(balance)), linetype="dotted") +
  geom_point(color="black", size=3) + 
  geom_vline(xintercept = 0, color="black") + 
  labs(title="Balance por covariables (Mujeres)" , x=" ", y = " ") +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        axis.text.x=element_text(size = 5))
g1

# Valor p
g2 <- test2 %>%  ggplot(aes(y=factor(balance), x=as.numeric(pvalue))) +
  geom_point(color="black", size=1, shape=17) + 
  labs(title="Valor P", x=" ", y = " ") +
  geom_vline(xintercept = 0.01, color="black", linetype="dashed", size=0.5, alpha=0.7) +
  geom_vline(xintercept = 0.05, color="blue", linetype="dashed", size=0.5, alpha=0.7) +
  geom_vline(xintercept = 0.1, color="red", linetype="dashed", size=0.5, alpha=0.7) +
  geom_text(aes(label=pvalue), hjust=0.5, vjust=-0.5, size=2.5) + 
  scale_x_continuous(breaks=c(0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)) + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 5))
g2

# Unimos todos los gráficos
ggarrange(g1, g2, ncol=2, nrow=1)


# Efecto del tratamiento:
t.test(lexptot ~ dfmfd, var.equal = TRUE, data=exp) # Mujeres 
t.test(lexptot ~ dmmfd, var.equal = TRUE, data=exp) # Hombres

# Impacto de participar en el programa (ITT)
# ¿Cómo interpretamos estos resultados?
# ¿Cómo interpretamos variables independientes binarias?
 
m1 <- lm(lexptot ~ dfmfd, data=exp, weights=weight)
screenreg(m1)

m2 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead, data=exp, weights=weight)

m3 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess, data=exp, weights=weight)

m4 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
     pcirr + rice + wheat + milk + oil + egg, data=exp, weights=weight)

screenreg(l=list(m1, m2, m3, m4), caption="Impacto del programa de microcréditos", float.pos="h" , digits = 3, 
          caption.above = TRUE,  include.ci = FALSE, stars = c(0.01, 0.05, 0.1), fsingle.row = T)

# ¿Cómo interpretamos el R2 y R2 ajutados? 

# Evalúemos la hipótesis de que el beta del tratamiento es igual a 0
# H0: 
# H1: 

car::linearHypothesis(m1, c("dfmfd=0"))
car::linearHypothesis(m2, c("dfmfd=0"))
car::linearHypothesis(m3, c("dfmfd=0"))
car::linearHypothesis(m4, c("dfmfd=0"))


# Evalúemos la hipótesis de que sexhead y agehead son iguales a 0 a partir del modelo 4
# H0: 
# H1: 
car::linearHypothesis(m4, c("sexhead=0", "agehead=0"))

# Evalúemos la hipótesis de que sexhead y agehead son iguales partir del modelo 4
# H0: 
# H1: 
car::linearHypothesis(m4, c("sexhead-agehead=0"))

# Comparemos modelos (recuerde que siempre es R vs NR)

# m1 vs m2
anova(m1, m2)

# m2 vs m3
anova(m2, m3)

# m3 vs m4
anova(m3, m4)

#**************************************************************************************************************************/
# 5. Supuestos de OLS ----------------------------------------------------------------
#**************************************************************************************************************************/

# Un buen método de estimación debería cumplir con tres condiciones:

# a. Insesgado: entrega estimaciones que son correctas en promedio, en otras palabras, un estadístico es inses- gado si la media de su distribución muestral es igual al parámetro que se busca estimar.

# b. Eficiente: las estimaciones de esta distribución muestral tienen una varianza pequeña, es decir que los valores están concentrados en torno al parámetro poblacional.

# c. Consistente: las estimaciones del método convergegn al parámetro verdadero cuando el tamaño muestral crece.

# Aprovechemos este modelo para revisar los supuestos de OLS 

screenreg(m4)

# Para el anaálisis de residuos obtendremos los residuos del modelo, 
# los residuos estandarizados y los studentizados

exp$pred <- predict(m4)
exp$resid <- residuals(m4)
exp$resid_std <- ls.diag(m4)$std.res 
exp$resid_t <- ls.diag(m4)$stud.res

# S0: Esperanza condicional del error es cero. Independencia: 𝐸(e|x_i)=0, i=1,...,n

# El valor de un residuo para una observación no puede depender del residuo de otra observación. 
# Cada residuo es independiente entre sí para que no tengamos problemas de eficiencia en la estimación.

# S1: Muestra es independiente e identicamente distribuida (idd)
# Variables con distribución de probabilidad 
# Variables independientes entre ellas

ggplot(exp, aes(x=nh, y=resid))+
  geom_point(na.rm = TRUE) +
  labs(title="Independence Plot",
       x="ID",
       y="Residuos") +
  theme_light()

# S2: Linealidad de los datos 

g1 <- ggplot(exp, aes(pred, resid)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(se = FALSE) +
  labs(title="Residuos vs Predichos", 
       x="Predichos", y="Residuos") +
  theme_light()

g1

# S3: Homocedasticidad 
#El témino error (e) tiene la misma varianza a través de los distintos valores de x’s (y sus combinaciones).
# Este supuesto hace alusión a la eficiencia de nuestra estimación, en otras palabras, 
# podemos obtener una solución analítica para el error estándar en el modelo OLS.
# Si el supuesto no se cumple nuestros valores p no serán los más pequeños posibles 
# dado que las predicciones de Y son más variables dada algunos valores de x’s. 
# ¿En qué se reduce esto en la práctica?

# Residuos vs Predichos
g1 <- ggplot(exp, aes(pred, resid)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(se = FALSE) +
  labs(title="Residuos vs Predichos", 
       x="Predichos", y="Residuos") +
  theme_light()

# Residuos estandarizados vs Predichos
g2 <- ggplot(exp, aes(pred, resid_std )) +
  geom_point() +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  geom_smooth(se = FALSE) +
  labs(title="Residuos Estandarizados vs Predichos", 
       x="Predichos", y="Residuos Estandarizados") +
  theme_light()

g3 <- ggplot(exp, aes(pred, sqrt(abs(resid)))) +
  stat_smooth(method="loess", na.rm = TRUE) +
  geom_point() +
  labs(title="Scale-Location",
       x="Predichos", y=expression(sqrt("|Standardized residuals|"))) +
  theme_light()

ggarrange(g1, g2, g3, ncol = 3)

bptest(m4)

# S4: Normalidad en los residuos 
ggplot(m4, aes(qqnorm(.stdresid)[[1]], .stdresid))+
  geom_point(na.rm = TRUE) +
  geom_abline(aes(qqline(.stdresid))) +
  labs(title="Normal Q-Q",
       x="Theoretical Quantiles",
       y="Standardized Residuals") +
  theme_light()

s <- shapiro.test(exp$resid)$p.value
l <- lillie.test(exp$resid)$p.value

# S4: No hay multicolinealidad perfecta
# Las variables independientes del modelo no están relacionadas de forma perfecta entre sí. 
# Esto puede producir problemas de eficiencia en nuestras estimaciones.

car::vif(m4)

# S5: Outliers poco probables
# No hay casos que alteran el valor de un coeficiente de regresión. 
# Por ejemplo, ¿en qué caso observaríamos un punto de influencia?

# Valores atípicos: en la regresión lineal, un valor atípico es una observación con un residuo grande.

# Apalancamiento: una observación con un valor extremo en una variable de predicción se denomina punto con alto apalancamiento. 
# El apalancamiento es una medida de cuánto se desvía una observación de la media de esa variable.

# Influencia: Se dice que una observación es influyente si la eliminación de la observación cambia sustancialmente la estimación de los coeficientes.
# Una observación influyente es atípica y genera apalancamiento
influencePlot(m3)

# Distancia de Cooks´s
ggplot(m4, aes(seq_along(.cooksd), .cooksd))+
  geom_bar(stat="identity", position="identity") +
  labs(x="Obs. Number", y="Distancia", 
       title="Distancia de Cook's") +
  theme_light()

# Apalancamiento
ggplot(m4, aes(.hat, .stdresid))+
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  labs(x="Apalancamiento", y="Residuo std", 
       title="Residuo vs apalancamiento") +
  scale_size_continuous("Cook's Distance", range=c(1,5)) +
  theme_light() + theme(legend.position="bottom")
  

ggplot(m4, aes(.hat, .cooksd))+
  geom_point(na.rm=TRUE)+
  stat_smooth(method="loess", na.rm=TRUE) +
  geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") +
  labs(x="Apalancamiento hii", y="Distancia de Cooks`s", 
       title="Distancia de Cook`s vs Leverage hii/(1-hii)") +
  theme_light() + theme(legend.position="bottom")
  
  
car::outlierTest(m4)
car::influencePlot(m4)

par(mfrow = c(2, 2))
plot(m4)

### Otros diagnósticos 

# Considerar autocorrelación de los residuos
durbinWatsonTest(m3)

# Especificación del modelo
# Un error de especificación del modelo puede ocurrir cuando una o más variables relevantes
# se omiten del modelo o una o más variables irrelevantes se incluyen en el modelo

par(mfrow = c(4, 3))
car::avPlots(m4) 

############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/
