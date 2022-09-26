############################################################################################################################/
##################### Ejemplos semana 3 ###################################################################
############################################################################################################################/
# Dudas: 
#José Daniel Conejeros (jdconejeros@uc.cl) 
#Raimundo Eyzaguirre (reyzaguirre1@uc.cl)
############################################################################################################################/

## 0. Settings ----
options(scipen = 999)
options(max.print = 999)
rm(list=ls())

# install.packages("") # Sino lo tiene disponible
library(dplyr) # Manipulación 
library(tidyr) # Manipulación 
library(AER) # Contiene los datos 
library(np) # Contiene los datos de wage
library(wooldridge) # Contiene datos
library(estimatr) # Regresiones con errores estándar robustos
library(ggplot2) # Visualización de datos 
library(kableExtra) # Manejo de tablas
library(performance) # Supuestos de regresión
library(car) # Supuestos y otros test
library(texreg) # Reporte de regresiones
library(Ecdat) # Funciones para evaluar supuestos del modelo

############################################################################################################################/

## 1. Estudio experimental (STAR)  ----

### 1.1 Datos ----

# Datos
data(STAR)
colnames(STAR)
glimpse(STAR)

# Generamos una data de respaldo solo con las modificaciones a la data
df <- STAR %>% 
  tidyr::drop_na(stark, readk, mathk) %>% 
  mutate(str=if_else(stark=="small", 1, 0), 
         free_lunch=if_else(lunchk=="free", 1, 0), 
         male=if_else(gender=="male", 1, 0), 
         black=if_else(ethnicity=="afam", 1, 0)) %>% 
  rowwise() %>% 
  mutate(tscorek=(readk + mathk))

### 1.2 Regresión simple ----
summary(lm_robust(tscorek ~ str, data = df, se_type = "stata"))

############################################################################################################################/

## 2. Estudio observacional rendimiento (California) ----

### 2.1 Datos ----

# Datos
data(CASchools)
colnames(CASchools)
glimpse(CASchools)

# Generamos una data de respaldo solo con las modificaciones a la data
df2 <- CASchools %>% 
  mutate(ratio=students/teachers,
         small=if_else(ratio<20, 1, 0)) %>% 
  rowwise() %>% 
  mutate(tscorek=mean(c(read, math)))

df2 <- df2 %>% dplyr::select(district, tscorek, ratio, small, income)

### 2.2 Regresión simple ----

# Modelo de regresión simple con SE robusto
summary(lm_robust(tscorek ~ ratio, data = df2, se_type = "stata")) 

### 2.3 Regresión múltiple ----

# Modelo de regresión simple con SE robusto
summary(lm_robust(tscorek ~ ratio + income, data = df2, se_type = "stata")) 

############################################################################################################################/

## 3. Control prenatal ----

### 3.1 Datos ----

# Datos
df3 <- bwght2

data(df3)
colnames(df3)
glimpse(df3)

# Ajustamos la data
# Fuente: https://justinmshea.github.io/wooldridge/reference/bwght2.html
df3 <- df3 %>% mutate(monpre_bin=if_else(monpre<=2, 1, 0)) %>% drop_na(monpre_bin) 
df3 <- df3 %>% select(-monpre, -mwhte, -lbw, -moth, -fwhte, -foth, -lbwght) %>% relocate(bwght, monpre_bin, cigs)

### 3.2 Descriptivos ----
vars <- c("bwght", "monpre_bin", "cigs")
tab <- c()

for(i in vars){
  x <- df %>% 
    dplyr::summarise(Obs=n(),
                     Media=mean(get(as.name(i)), na.rm=TRUE),
                     Std=sd(get(as.name(i)), na.rm=TRUE),
                     Min=min(get(as.name(i)), na.rm=TRUE),
                     Max=max(get(as.name(i)), na.rm=TRUE)) %>%
    mutate(Variable=i) %>%
    relocate(Variable)
  
  tab <- tab %>% rbind(x)
}

knitr::kable(tab, booktabs=TRUE, align=c("lccccc"), linesep="", format="html", digits = 4,
             row.names=FALSE, ) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive")) %>% 
  row_spec(0, bold=TRUE, background =  "#586CC4", color= "white") %>% 
  row_spec(1:3, background = "white")

### 3.3 Sesgo por variable omitida ----

# Correlación entre consumo de cigarrillos (omitida) y tratamiento
cor.test(df$monpre_bin, df$cigs, method = "pearson")

df %>% 
  group_by(monpre_bin) %>% 
  dplyr::summarise(Obs=n(),
                   Media=mean(cigs, na.rm=TRUE),
                   Std=sd(cigs, na.rm=TRUE),
                   Min=min(cigs, na.rm=TRUE),
                   Max=max(cigs, na.rm=TRUE))

### 3.4 Regresión múltiple ----
summary(lm_robust(bwght ~ monpre_bin, data = df, se_type = "stata"))

summary(lm_robust(bwght ~ monpre_bin + cigs, data = df, se_type = "stata"))

############################################################################################################################/

## 4. Modelo de regresión múltiple: Peso del niño/a al nacer post tratamiento  ----

### 4.1 Datos ----

# Datos
# Fuente: https://justinmshea.github.io/wooldridge/reference/bwght2.html
df <- bwght2

colnames(df)
glimpse(df)

# Ajustamos la data para los análisis
df <- df %>% mutate(monpre_bin=if_else(monpre<=2, 1, 0)) %>% drop_na(monpre_bin) 
df <- df %>% select(-monpre, -mwhte, -lbw, -moth, -fwhte, -foth, -lbwght) %>% relocate(bwght, monpre_bin, cigs)

### 4.2 Regresión múltiple ----
m1 <- lm_robust(bwght ~ ., data = df, se_type = "stata")
summary(m1) # ¿Qué representan los coeficientes cigs, mage, male.

### 4.3 Bodad de ajuste: R2 ----

# ¿Qué nos dice el R2 y el R2 del modelo
m1[["r.squared"]]
m1[["adj.r.squared"]]


### 4.4 Supuestos del modelo ----

check_model(lm(bwght ~ ., data = df)) # Identifique el supuesto correspondiente a cada panel

############################################################################################################################/

## 5. Variables Dummy: brechas de género en salario ----

### 5.1 Datos ----

# Datos
dw <- wage1 
colnames(dw)
glimpse(dw)

# Ajustamos lois datos
dw <- dw %>% mutate(male=1-female) %>% relocate(wage, female, male)

### 5.2 Regresión simple ----

# Modelo de regresión simple con SE robusto para mujeres
m2 <- lm_robust(wage ~ female, data = dw, se_type = "stata")
summary(m2)

# ¿Qué pasa si agregamos la binaria para hombres?
m3 <- lm_robust(wage ~ female + male, data = dw, se_type = "stata")
summary(m3)

### 5.3 Regresión múltiple ----

# Ajustamos los datos para realizar el análisis
dw <- dw %>% mutate(
  lesscholl=if_else(educ<12, 1, 0), 
  highscholl=if_else(educ>=12 & educ<16, 1, 0),
  college=if_else(educ>=16, 1, 0)
)

dw %>% select(wage, female, male, educ, lesscholl, highscholl, college) %>% glimpse()

# Modelo de regresión simple con SE robusto con variable de dos o más categorías 
m4 <- lm_robust(wage ~  highscholl + college, data = dw, se_type = "stata")
summary(m4) #¿Cómo interpretamos estos resultados?

### 5.4 Regresión múltiple II ----

# Podemos generar varios modelos de regresión
m3a <- lm_robust(wage ~ female, data = dw, se_type = "stata")
m3b <- lm_robust(wage ~ male, data = dw, se_type = "stata")
m3c <- lm_robust(wage ~ educ, data = dw, se_type = "stata")
m3d <- lm_robust(wage ~ exper, data = dw, se_type = "stata")
m3e <- lm_robust(wage ~ female + educ + exper + tenure, data = dw, se_type = "stata")

# Con la librería texreg podemos interpretar nuestros resultados
screenreg(list(m3a, m3b, m3c, m3d, m3e), digits = 3, include.ci = FALSE,
          caption.above = TRUE, single.row = TRUE,
          caption = "Variable dependiente: wage")

############################################################################################################################/

## 6. Test de hipótesis ----

### 3.1 Datos ----
# Resultados al nacer (Apgar al minuto) y variables explicativas
# `omaps:` one minute apgar score
# `cigs:` avg cigarettes per day
# `drink:` avg drinks per week
# `meduc:` mother's educ, years
# `feduc:` father's educ, years
# `male:` =1 if baby male

# Datos
df <- bwght2
colnames(df)
glimpse(df)


### 6.2 Modelo de regresión múltiple ----

m4 <- lm_robust(omaps ~  cigs + feduc + meduc + male, data = df, se_type = "stata")

# Redondeamos las cifras para ver claramente los números
m4$p.value <- round(m4$p.value, 3)
m4$conf.low <- round(m4$conf.low, 3)
m4$conf.high <- round(m4$conf.high, 3)

# Revisamos los resultados del modelo
summary(m4) # ¿Cómo interpretamos los coeficientes? ¿Son significativos?

### 6.3 Test de hipótesis ----

# Queremos evaluar la siguiente hipótesis
#H_0: beta_{meduc}= beta_{feduc}=0
#H_A: beta_{meduc}!= 0 y/o beta_{feduc}!=0
linearHypothesis(m4, c("feduc = 0", "meduc = 0"))

# Clave: correlación entre ambas variables aumenta los errores estándar en los coeficientes de estas variables
cor.test(df$feduc, df$meduc, method = "pearson")

### 6.4 Regresión múltiple: Test F para comparar modelos ----

# F-test mira el aumento en el R2 cuando agregamos variables de interés. En otras palabras, medirá si estas variables adicionales agregan algo al modelo.

# Comparemos los siguientes modelos
# na.omit(df) nos permite que ambos modelos tengan las mismas observaciones y sean comparables
m4a <- lm_robust(omaps ~ cigs + male, data = na.omit(df), se_type = "stata")
m4b <- lm_robust(omaps ~ cigs + male + feduc + meduc, data = na.omit(df), se_type = "stata")

# Reportamos nuestros resultados
screenreg(list(m4a, m4b), digits = 3, include.ci = FALSE,
          caption.above = TRUE, single.row = TRUE,
          custom.model.names = c("Modelo Restrigido", "Modelo NO Restringido"),
          caption = "Variable dependiente: wage")

# Aplicamos el test para comparar modelos
waldtest(m4a, m4b)

# También podemos comparar con un test de ANOVA pero para modelos sin error estándar robusto
m4a <- lm(omaps ~ cigs + male, data = na.omit(df))
m4b <- lm(omaps ~ cigs + male + feduc + meduc, data = na.omit(df))

# Aplicamos el test
anova(m4a, m4b)

# Obtenemos resultados relativamente similares entre ambos test. 

############################################################################################################################/

## 7. Interacciones  ----

### 7.1 Datos ----

# Datos
dw <- wage1 # Datos sobre ingresos

colnames(dw)
glimpse(dw)

dw <- dw %>% mutate(male=1-female) %>% relocate(wage, female, male) # Ajustamos el género como binaria 

### 7.2 Regresión múltiple ----

m1 <- lm_robust(wage ~ female + educ, data = dw, se_type = "stata")
summary(m1)

### 7.3 Regresión múltiple con interacciones ----

# Ajustamos la data para los análisis
dw <- dw %>% mutate(female_educ=educ*female)

# Estimamos el modelo 
m2 <- lm_robust(wage ~ female + educ + female_educ, data = dw, se_type = "stata")
summary(m2) # ¿Cómo interpretamos los resultados?

### 7.4 Estudio de caso ----

# Diferencias en accidentes fatales de tránsito entre lugares con distinta regulación para el consumo de cerveza

# - `state`: state ID code
# - `year`: year
# - `mrall`: traffic fatality rate (deaths per 10000)
# - `beertax`: tax on case of beer
# - `jaild`: mandatory jail sentence?
#   - `comserd`: mandatory community service?
#   - `vmiles`: average miles per driver
# - `unrate`: unemployment rate
# - `perinc`: per capita personal income

# Ajustamos los datos
dfat <- Ecdat::Fatality %>% filter(year==1988) %>% 
  mutate(jaild=if_else(jaild=="yes", 1, 0),
         comserd=if_else(comserd=="yes", 1, 0))

# Veamos estadísticos descriptivos de las variables
vars <- c("mrall", "beertax", "jaild", "comserd", "vmiles", "unrate", "perinc")
tab <- c()

for(i in vars){
  x <- dfat %>% 
    dplyr::summarise(Obs=n(),
                     Media=mean(get(as.name(i)), na.rm=TRUE),
                     Std=sd(get(as.name(i)), na.rm=TRUE),
                     Min=min(get(as.name(i)), na.rm=TRUE),
                     Max=max(get(as.name(i)), na.rm=TRUE)) %>% 
    mutate(Variable=i) %>%
    relocate(Variable)
  
  tab <- tab %>% rbind(x)
}

tab # Vemos nuestro resultado 

# Apliquemos nuestros modelos de regresión
m3a <- lm_robust(mrall ~ jaild, data = dfat, se_type = "stata")
m3b <- lm_robust(mrall ~ jaild + beertax + comserd + vmiles + unrate + perinc, 
                 data = dfat, se_type = "stata")
m3c <- lm_robust(mrall ~ jaild + beertax + comserd + vmiles + unrate + perinc + jaild:beertax, 
                 data = dfat, se_type = "stata") # Este modelo incluye el efecto interacción 

# Vemos nuestros resultados
screenreg(list(m3a, m3b, m3c), digits = 3, include.ci = FALSE,
          caption.above = TRUE, single.row = TRUE,
          caption = "Variable dependiente mrall: traffic fatality")

############################################################################################################################/

## 8. Logaritmos ----

### 8.1 Datos ----

# Datos
dw2 <- wage2
colnames(dw2)
glimpse(dw2)

# Relación entre puntaje académico y salarios
ggplot(dw2, aes(x = wage, y =KWW)) +
  geom_point(alpha=0.5) +
  labs(y="Test score", x="Wage") +
  theme_classic()

### 8.2 Regresión simple ----

# Ajustamos el logaritmo natural
dw2 <- dw2 %>% mutate(lwage=log(wage)) # Logaritmo natural

# Modelo de regresión simple con SE robusto
m4 <- lm_robust(KWW ~ lwage, data=dw2, se_type = "stata")
summary(m4) # ¿Cómo interpretamos los resultados del modelo?

### 8.3 Regresión múltiple ----

# Volvamos al ejemplo de los salarios
m5 <- lm_robust(lwage ~ educ + exper + tenure + female, data=dw, se_type = "stata")
summary(m5) # ¿Cómo interpretamos los resultados del modelo?


############################################################################################################################/

## 9. Especificaciones cuadráticas ----

### 9.1 Datos ----

# Relación entre puntaje académico y salarios 
ggplot(dw2, aes(x = wage, y =IQ)) +
  geom_point(alpha=0.5) +
  labs(y="Test score", x="Wage") +
  theme_classic()


### 9.2 Modelo de regresión múltiple ----

# Ajustamos la variable 
dw2 <- dw2 %>% mutate(wage2=wage^2)
m6 <- lm_robust(IQ ~ wage + wage2, data=dw2, se_type = "stata")
summary(m6) # ¿Cómo interpretamos los resultados del modelo?


m7 <- lm_robust(IQ ~ wage, data=dw2, se_type = "stata")
summary(m7) # ¿Cómo interpretamos los resultados del modelo?

############################################################################################################################/
##################### FIN DEL EJEMPLO ###################################################################
############################################################################################################################/

