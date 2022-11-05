############################################################################################################################/
##################### Taller 6: DID y aplicaciones ML #####################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Uso y aplicaciones de DAGS 
# 2. Revisión DID
# 3. Primera aproximación a ML en un contexto de inferencia causal

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
              "texreg", "Publish", "broom", "ggpubr", "estimatr", "texreg", "corrplot", "scales",
              "caret", "MLmetrics", 
              "rpart", "rpart.plot",
              "rattle",
              "randomForest",
              "class",
              "e1071", "klaR", "kernlab", 
              "neuralnet", 
              "glmnet",
              "tidyverse"))

sessionInfo()

############################################################################################################################/
# I. Diferencia en Diferencia --------------------------------------------------
############################################################################################################################/

# En este ejemplo nos interesa evaluar el efecto causal de la policía sobre la delincuencia. 
# Tras un ataque terrorista contra el principal centro judío de Buenos Aires, Argentina, 
# en julio de 1994, todas las instituciones judías recibieron protección policial.

# Este suceso indujo una asignación geográfica de las fuerzas policiales que puede presumirse exógena 
# en una regresión sobre la delincuencia. Utilizando datos sobre la localización de los robos de coches 
# antes y después del atentado, se explora el efecto disuasivo de la policía.

## 1.1 Exploramos los datos --------------------------------------------------

# Importamos la base de datos
panel <- import("data/Panelmensual.dta") # Ajustar directorio propio

# Realizamos una exploración inicial
dim(panel)
colnames(panel) 
colnames(panel) <- tolower(colnames(panel))
str(panel)
View(panel)
glimpse(panel)

# Variables de interés:
# totrob: Número total de robos de autos por cuadra durante cada mes
# post: Dummy que identifica el tratamiento
# institu1: Dummy que identifica si hay una institución judia en el bloque

# ¿En qué formato está el panel de datos? ¿Long o Wide?

# Transformar la base a formato wide (es solo un ejemplo):
names <- c("post", "institu1", "totrob")
panel_wide <- panel %>%
    dplyr::select(observ, barrio, calle, mes, post, institu1, totrob) %>% 
    tidyr::pivot_wider(names_from = mes, values_from = all_of(names), names_sep="_") %>% 
    arrange(observ) %>% ungroup()

?tidyr::pivot_longer

View(panel_wide)


## 1.2 Diff in Diff manual --------------------------------------------------

# Exploremos la distribución general 
table(panel$post, panel$institu1, useNA = "ifany")

# Vamos a calcular los 4 puntos para la estimación de diferencia en diferencia 
# Después de la intervención
t1t = sapply(subset(panel, post == 1 & institu1 == 1, select=totrob), mean)
t1t
t1c = sapply(subset(panel, post == 1 & institu1 == 0, select=totrob), mean)
t1c

# Antes de la intervención: ¿Qué debería pasar con los valores para tratados y controles previo al tratamiento?
t0t = sapply(subset(panel, post == 0 & institu1 == 1, select=totrob), mean)
t0t

t0c = sapply(subset(panel, post == 0 & institu1 == 0, select=totrob), mean)
t0c

# Primeras diferencias 
dif1 = (t1t - t0t) # ¿Qué estamos controlando aquí?
dif2 = (t1c - t0c) # ¿Qué estamos controlando aquí?

dif1
dif2

# Segundas diferencias (3 formas)
(t0c - t1c) - (t0t - t1t) 
(t1t - t1c) - (t0t - t0c)
dif1 - dif2

## 1.3 Diff in Diff vía regresión --------------------------------------------------

# El modelo de regresión es el siguiente: 

#Yit =β0 +β1Iit1 +β2Di +δDi ·Iit1 +εit
# δDi = (t1t - t1c) - (t0t - t0c)

#Repliquemos el modelo A: 

#a. Sin incorporar efectos fijos: 
did <- lm(totrob ~ factor(inst1p), data = panel)
summary(did)

did_robusto <- lm_robust(totrob ~ factor(inst1p), data = panel, alpha=0.01)
screenreg(did_robusto, digits = 5)

#b. Incorporemos efectos fijos: ¿Por qué hacemos esto?
# Manual
modeloA <- lm_robust(totrob ~ factor(inst1p) + factor(mes)+ factor(observ), data = panel, alpha=0.01)
# Automatizado
modeloA <- lm_robust(totrob ~ factor(inst1p) + factor(mes), data = panel, fixed_effects = ~ observ, alpha=0.01)
modeloA <- lm_robust(totrob ~ factor(inst1p), data = panel, fixed_effects = ~ observ + mes, alpha=0.01)

# Reportamos
summary(modeloA)

# ¿Qué pasa si lo hacemos con sin errores estándar robustos?
m0 <- lm(totrob ~ factor(inst1p) + factor(mes)+ factor(observ), data = panel, alpha=0.01)
summary(m0)

# c. Podemos hacer un ejercicio equivalente especificando una interacción:
lmr_out1 <- lm(totrob ~ factor(institu1)*factor(post), data = panel, fixed_effects = ~ observ + mes, alpha=0.01)
lmr_out2 <- lm_robust(totrob ~ factor(institu1)*factor(post), data = panel, fixed_effects = ~ observ + mes, alpha=0.01)

screenreg(l=list(lmr_out1, lmr_out2), digits = 6)

# Construímos la interacción
panel$interaccion <- panel$institu1*panel$post
table(panel$inst1p, panel$interaccion)


## 1.4 Diff in Diff multivariado --------------------------------------------------

# Repliquemos el modelo B: Incluye la variable [inst3_1p] que captura el efecto de la presencia de la policía en los seis bloques que son contiguos en cualquier dirección a cada bloque ocupado por una institución vigilada. 
modeloB <- lm_robust(totrob ~ factor(inst1p) + factor(inst3_1p), data = panel, fixed_effects = ~ observ + mes, alpha=0.01)
summary(modeloB)
screenreg(modeloB, digits = 5)

# Repliquemos el modelo C: Incluye la variable [inst3_1p] más la variable [cuad2p] que toma el valor 1 durante el período posterior al ataque para todos los bloques que están a dos bloques de la institución judía más cercana.
modeloC <- lm_robust(totrob ~ factor(inst1p) + factor(inst3_1p) + factor(cuad2p), data = panel, fixed_effects = ~ observ + mes, alpha=0.01)
summary(modeloC)
screenreg(modeloC, digits = 5)

############################################################################################################################/
# II. Notas sobre Modelos de ML ----------------------------------------------------------------
############################################################################################################################/

# En el mundo de la econometría el enfoque está más marcado hacia la estimación de parámetros y la inferencia causal, en vez de 
# la predicción. A su vez, la predicción está pensando bajo un modelo paramétrico. Funcionamos a partir de modelos teóricos y 
# la teoría asintótica.

# En ML, en cambio, el foco está en ajustar el mejor modelo (paramétrico o no) con el objetivo de predecir de mejor manera 
# la variable de respuesta: predicción (regresión) y clasificación. Por lo que su uso es más práctico. Por lo que estos modelos
# vienen de los datos y no la teoría o supuestos del investigador/a. Lo clave está en entrenar y validar.

# Veamos una aplicación: 

# ML Supervisado (clasificación / Regresión)
# Regresión regularizada, random forest, regresión, redes neuronales, regresión logística, etc.

# ML no supervisado
# Kmeans, K-mode, Clustering, Análisis factorial, componentes principales.

# Los modelos ML siguen la siguiente secuencia: 
# 1. Pre-procesamiento de la data y extracción
# 2. Preparación de la data
# 3. Aplicamos un algoritmo ML en una data de entrenamiento
# 4. Iteramos hasta encontrar el mejor modelo
# 5. Seleccionamos y validamos el modelo en una data de validación
# 6. Evaluamos la precisión del modelo y realizamos mantención

# Necesitamos entrenar nuestros algoritmos: 
# Data-entrenamiento (70%- 80%)
# Data validación (30% - 20%)

# Precisión: falsos positivos. 
# Recall: verdaderos positivos. 
# F1: equilibrio entre precisión y sensibilidad.
# Accuracy: predicciones correctas entre todas las predicciones

# Hay que tener cuidado con: 

# Underfitting (Alto sesgo y baja varianza): Si nuestros datos de entrenamiento son muy pocos, nuestra máquina de aprendizaje no 
# será capaz de generalizar el conocimiento y estará incurriendo en bajo ajuste.

# Para reducir:
# a. Incrementar la complejidad del modelo
# b. Aumentar el número de covariables
# c. Eliminar el ruido de los datos
# d. Aumentar la duración del entrenamiento

# Overfitting (Alta varianza y bajo sesgo): Al entrenar demasiado o entrenar con datos “raros”, el modelo se ajusta a características 
# muy específicas de los datos de entrenamiento, lo que puede provocar que los datos nuevos queden mal clasificados.

# Para reducir:
# a. Aumentar los datos de entrenamiento
# b. Reducir la complejidad del modelo
# c. Disminuir la fase de entrenamiento
# d. Usar técnicas de regularización: Ridge y Lasso
# e. Usar dropout de redes neuronales para abordar el overfitting

# Para validar nuestro modelo
# Validación cruzada: Método estadístico para obtener una estimación confiable del rendimiento del modelo utilizando solo sus datos de entrenamiento.
# Nos sirve mucho para el Overfitting

# Validación cruzada k-iteraciones (k-fold cross-validation - K-FCV) consiste en dividir los datos en k subconjuntos de
# igual tamaño y repetir el procedimiento k veces de modo que cada vez, uno de los k subconjuntos 
# es el conjunto de validación y los otros k-1 subconjuntos forman el conjunto de entrenamiento.

# Otros: K-FCV estratificada, Leave-p-out cross-validation, Leave-one-out cross-validation, etc.


############################################################################################################################/
# Aplicación práctica ----------------------------------------------------------------
############################################################################################################################/

# Veamos una aplicación sencilla
data_cancer <- import("data/Cancer.csv")

# Realizamos algunos ajustes a la variable de respuesta 
data_cancer <- data_cancer %>% 
  dplyr::select(-id, -V33) %>% 
  mutate(diagnosis = factor(ifelse(diagnosis == "M", "0", "1"))) %>% 
  drop_na() %>% 
  rename( "concavepointsmean" = `concave points_mean`)  %>% 
  dplyr::select(diagnosis, contains("mean"))

# Observemos correlación entre variables
data_cancer %>% 
  dplyr::select(-diagnosis) %>% 
  cor() %>% 
  corrplot()

# Generamos base de datos de entrenamiento y testeo
set.seed(2021)
train <- slice_sample(data_cancer, prop = 0.7)
test <- anti_join(data_cancer, train)

############################################################################################################################/
## 2.1 Regresión Logística ----------------------------------------------------------------
############################################################################################################################/

# Ajustamos regresión logística
model_log <- glm(diagnosis ~ ., data = train,
                 family = binomial(link = "logit"))
summary(model_log)

model_log <- step(model_log)
summary(model_log)

# Calidad de ajuste
pred_log <- as.numeric(predict(model_log, train, type = "response") > 0.5)
error_log <- mean(pred_log != train$diagnosis)

Precision(train$diagnosis, pred_log, positive = 1)   
Accuracy(train$diagnosis, pred_log)

F1_Score(train$diagnosis, pred_log, positive=1)

# Validacion cruzada
traincontrol <- trainControl(method="cv", number=10, p=0.7)
model_log_cv <- train(diagnosis ~ ., data = train,
                      method = "glmStepAIC",
                      trControl= traincontrol)
summary(model_log_cv)
predcv_log <- predict(model_log_cv, newdata = test)
errorcv_log <- mean(predcv_log != test$diagnosis)

Precision(test$diagnosis, predcv_log, positive = 1)
Accuracy(test$diagnosis, predcv_log)
F1_Score(test$diagnosis, predcv_log, positive = 1)

## Validación del modelo
predval_log <- as.numeric(predict(model_log, newdata = test, type = "response") > 0.5)
errorval_log <- mean(predval_log != test$diagnosis)

predval_log 
errorval_log

Precision(test$diagnosis, predval_log, positive=1)
Accuracy(test$diagnosis, predval_log)
Fval_log <- F1_Score(test$diagnosis, predval_log, positive=1)

Fval_log 

############################################################################################################################/
## 2.2 Árbol de decisión ----------------------------------------------------------------
############################################################################################################################/

model_tree <- rpart(diagnosis ~ ., data = train)
fancyRpartPlot(model_tree)
rpart.rules(model_tree, cover=TRUE)

## Calidad de ajuste
pred_tree <- as.numeric(predict(model_tree, train)[,2] > 0.5)
error_tree <- mean(pred_tree!= train$diagnosis)

### Validación cruzada
model_tree_cv <- train(diagnosis ~ ., data = train,
                       method = "rpart",
                       trControl= traincontrol)

## Validación del modelo
predval_tree <- as.numeric(predict(model_tree, newdata = test)[,2] > 0.5)
errorval_tree <- mean(predval_tree != test$diagnosis)

predval_tree
errorval_tree

Precision(test$diagnosis,predval_tree, positive = 1)
Accuracy(test$diagnosis,predval_tree)

F1_score_tree <- F1_Score(test$diagnosis,predval_tree, positive = 1)

F1_score_tree

############################################################################################################################/
## 2.3 Random Forest ----------------------------------------------------------------
############################################################################################################################/

set.seed(2021)
model_rf <- randomForest(diagnosis ~ ., data = train,
                         ntree = 1000) #Ajustar el número de árboles. 
varImpPlot(model_rf)

## Calidad de ajuste
model_tree_rf <- train(diagnosis ~ ., data = train,
                       method = "rf", ntree = 1000,
                       trControl= traincontrol)

# valores de los parámetros
grilla_rf <- expand.grid(mtry=1:20) # Número de variables 

set.seed(2021)
model_tree_rf_grilla <- train(diagnosis ~ ., data = train,
                              method = "rf", 
                              tuneGrid = grilla_rf,
                              trControl= traincontrol)
model_tree_rf_grilla

#TEST 
# # Manual Search(control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")(tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))(modellist <- list()(for (ntree in c(1000, 1500, 2000, 2500)) {(	set.seed(seed)(	fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)(	key <- toString(ntree)(	modellist[[key]] <- fit(}(# compare results(results <- resamples(modellist)(summary(results)(dotplot(results))
# 
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

## Validación del modelo
pred_rf_test <- predict(model_tree_rf_grilla, newdata = test)
error_rf_test <- mean(pred_rf_test != test$diagnosis)

pred_rf_test
error_rf_test

Precision(test$diagnosis, pred_rf_test, positive = 1)
Accuracy(test$diagnosis, pred_rf_test)

F1_score_rf <- F1_Score(test$diagnosis, pred_rf_test, positive = 1)

F1_score_rf

# Problemas de sobreajuste del modelo. 

############################################################################################################################/
## 2.4 KNN (vecinos más cercanos) ----------------------------------------------------------------
############################################################################################################################/

### Validación cruzada
grilla_knn <- expand.grid(k=1:40)

model_knn_cv <- train(diagnosis ~., data=train, method="knn", 
                      trControl=trainControl(method="cv", number=10, p=0.7)) 

set.seed(2021)
model_knn_cv_grilla <- train(diagnosis ~., data=train, method="knn", 
                             trControl=trainControl(method="cv", number=5, p=0.7),
                             tuneGrid=grilla_knn) ## 

model_knn_cv_grilla

plot(model_knn_cv_grilla)

## Sugiere usar K=7

model_knn <- knn(train[,-1], train[,-1], train$diagnosis, k = 7)
model_knn

## Calidad del ajuste

### Matriz de confusion
table(train$diagnosis, model_knn)
mean(train$diagnosis == model_knn)
Accuracy(train$diagnosis, model_knn)

### Validación
model_knn_test <- knn(train[,-1], test[,-1], train$diagnosis, k = 7)
error_knn <- mean(model_knn_test != test$diagnosis)

model_knn_test
error_knn

Precision(test$diagnosis, model_knn_test, positive = 1)
Accuracy(test$diagnosis, model_knn_test)

F1_score_knn <- F1_Score(test$diagnosis, model_knn_test, positive = 1)

F1_score_knn

############################################################################################################################/
## 2.5 Naive Bayes----------------------------------------------------------------
############################################################################################################################/

# Naive Bayes
model_NB <- naiveBayes(diagnosis ~., data=train)

## Calidad de ajuste
predval_NB <- predict(model_NB, train)
error_NB <- mean( predval_NB != train$diagnosis)

predval_NB
error_NB 

ConfusionMatrix(predval_NB,train$diagnosis)

## Validación cruzada
model_NB_cv <- train(diagnosis ~., data=train, method="nb", 
                     trControl=trainControl(method="cv", number=5, p=0.7))

## Validación
pred_NB <- predict(model_NB,test)
error_NB <-   mean(pred_NB != test$diagnosis)

pred_NB
error_NB

Precision(test$diagnosis, pred_NB, positive = 1)
Accuracy(test$diagnosis, pred_NB)

F_score_NB <- F1_Score(test$diagnosis, pred_NB, positive = 1)
F_score_NB

############################################################################################################################/
## 2.6 SVM ----------------------------------------------------------------
############################################################################################################################/

?svm # Hiperplanos para separar a distintas categorías o para predecir en regresión. 
# kernal=cambia la forma del plano: lineal, polinomial. 
model_svm <- svm(diagnosis ~., data=train) # por defecto kernel radial
summary(model_svm)

# Grilla de kernels
grilla_svm <- expand.grid(kernel = c("radial", "linear", "polynomial", "sigmoid"))

train(diagnosis ~., data=train, method="svmRadial", 
      trControl=trainControl(method="cv", number=5, p=0.8))

train(diagnosis ~., data=train, method="svmPoly", 
      trControl=trainControl(method="cv", number=5, p=0.8))

train(diagnosis ~., data=train, method="svmLinear", 
      trControl=trainControl(method="cv", number=5, p=0.8))

# El mejor kernel es el polinomial 

# Exploramos el modelo
set.seed(2021)
model_svm_cv <-train(diagnosis ~., data=train, method="svmPoly", 
                     trControl=trainControl(method="cv", number=5, p=0.8))

# Validación del modelo
pred_svm <- predict(model_svm_cv, newdata = test)
score_svm <- Precision(test$diagnosis, pred_svm, positive=1)

Precision(test$diagnosis, pred_svm, positive=1)
Accuracy(test$diagnosis, pred_svm)

f1_svm <- F1_Score(test$diagnosis, pred_svm, positive=1)
f1_svm

############################################################################################################################/
## 2.7 Red neuronal ----------------------------------------------------------------
############################################################################################################################/

?neuralnet
model_nr <- neuralnet(factor(diagnosis) ~., data=train, hidden=c(3,2,1), linear.output=FALSE)
plot(model_nr)
summary(model_nr)

# Calidad de ajuste 
train_pred <- as.numeric(predict(model_nr, train)[,2] > 0.5)
train_score <- Precision(train$diagnosis, train_pred)

# Validación
pred_rn <- as.numeric(predict(model_nr, test)[,2] > 0.5)

Accuracy(test$diagnosis, pred_rn)

f1_rn <- F1_Score(test$diagnosis, pred_rn, positive=1)
f1_rn

############################################################################################################################/
## 2.8 Resumen resultados ----------------------------------------------------------------
############################################################################################################################/

# Log
Fval_log 

# DT
F1_score_tree

# RF
F1_score_rf

# KNN
F1_score_knn

# NB
F_score_NB

# SVM
f1_svm

# RN
f1_rn

############################################################################################################################/
# III. Regularización ----------------------------------------------------------------
############################################################################################################################/

# La regresión lineal múltiple es un método estadístico que trata de modelar la relación entre una 
# variable continua y dos o más variables independientes mediante el ajuste de una ecuación lineal. 
# Tres de las limitaciones de OLS son:

# a. Se ven perjudicados por la incorporación de predictores correlacionados.
# b. No realizan selección de predictores, todos los predictores se incorporan en el modelo aunque no aporten información relevante. 
# Esto suele complicar la interpretación del modelo y reducir su capacidad predictiva.
# c. No pueden ajustarse cuando el número de predictores es superior al número de observaciones.

# Estrategias para abordar este problema: 

# 1. Subset selection: utilizar un proceso iterativo que vaya descartando los predictores menos relevantes.
# Forward stepwise selection 
# Backward stepwise selection
# Hibrid (double) Stepwise Selection

# 2. Regularización Ridge, Lasso o Elastic Net: estos métodos fuerzan a que los coeficientes del modelo 
# tiendan a cero, minimizando así el riesgo de overfitting, reduciendo varianza, 
# atenuado el efecto de la correlación entre predictores y reduciendo la influencia en el modelo de los predictores menos relevantes.

# Ajustamos un modelo incluyendo todos los predictores pero aplicando un costo/penalización que fuerce a los coeficientes para que tiendan a 0.
# Esto minimiza el impacto de predictores irrelevantes en el modelo. 

# Ojo que estos métodos requieren que normalicemos o estandaricemos las variables.

# Lasso es una variante de regularización: Fuerza a todos los coeficientes tiendan a 0.
# Ridge: reduce de forma proporcional todos los coeficientes, pero sin que ellos lleguen a 0. Esto es un problema de interpretación.
# Elastic net: combina ambas aplicaciones. 

# En estos modelos vamos a tener que identificar un hiperparámetro de regularización. Para esto se puede aplicar validación cruzada. 

# 3. Reducción de dimensionalidad: crean un número reducido de nuevos predictores (componentes o factores) 
# a partir de combinaciones lineales o no lineales de las variables originales y con ellas se ajusta el modelo.

# Concretamente estos métodos nos ayuda a aplicar concretamente el criterio de parsimonia. Ya veremos su utilidad en el contexto de regresión.

# Veamos una aplicación básica de LASSO 
# Ejemplo adaptado de: Khandker, S. R., Koolwal, G. B., & Samad, H. A. (2009). Handbook on impact evaluation: quantitative methods and practices. World Bank Publications.

# Vamos a evaluar los impactos de un programa de microcréditos sobre los gastos per-cápita de los hogares. 

exp <- import("data/hh_98.dta")
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

# Seleccionamos las variables de interés
exp1 <- exp %>% dplyr::select(lexptot, agehead, educhead,
                      lnland, vaccess, pcirr, rice, wheat, milk, oil, egg) %>% 
  drop_na()

head(exp1)

# Vamos a agregar una variable aleatoria con una distribución normal
exp1$var_std <- rnorm(nrow(exp1), mean=0, sd=1) 

head(exp1)

# Generamos base de datos de entrenamiento y testeo
set.seed(2021)
train <- slice_sample(exp1, prop = 0.7)
test <- anti_join(exp1, train)

# Modelo OLS
modelo <- lm(lexptot ~ ., data = train)
summary(modelo)

df_coeficientes <- modelo$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

pred_reg <- predict(modelo, newdata = train)

train_mse <- mean((pred_reg - train$lexptot)^2)
paste("MSE", train_mse)

pred_reg2 <- predict(modelo, newdata = test)

train_mse <- mean((pred_reg2 - test$lexptot)^2)
paste("MSE", train_mse)

# Selection
step(object    = lm(lexptot ~ ., data = train),
     direction = "backward")

step(object    = lm(lexptot ~ ., data = train),
     direction = "forward")

step(object    = lm(lexptot ~ ., data = train),
     direction = "both")

# Stepwise Selection
modelo <- step(
  object    = lm(lexptot ~ ., data = train),
  direction = "backward",
  scope     = list(upper = ~., lower = ~1),
  trace     = FALSE)

summary(modelo)

paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))

pred_reg <- predict(modelo, newdata = train)

train_mse <- mean((pred_reg - train$lexptot)^2)
paste("MSE", train_mse)

pred_reg2 <- predict(modelo, newdata = test)

train_mse <- mean((pred_reg2 - test$lexptot)^2)
paste("MSE", train_mse)


# LASSO

# Tenemos que dejar matrices de entrenamiento y test
x_train <- model.matrix(lexptot~., data = train)[, -1]
y_train <- train$lexptot

x_test <- model.matrix(lexptot~., data = test)[, -1]
y_test <- test$lexptot

# Ajustamos una regresión LASSO
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

summary(modelo)

# Extraemos los coeficientes
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )
regularizacion

# Gráfico de trayectorias
regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "left")

# A medida que aumenta el valor de lambda, la regularización es mayor y más predictores quedan excluidos (su coeficiente es 0).

# Podemos identificar el mejor lambda a partir de una validación cruzada
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

print(cv_error)

plot(cv_error)

coef(cv_error, s = "lambda.min")

paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

# Ajusto con el lambda óptimo
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Extraigo los coeficientes más relevantes
df_coeficientes %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 

# Data entrenamiento
pred_train <- predict(modelo, newx = x_train)
train_mse <- mean((pred_train - y_train)^2)
paste("Error (mse) de entrenamiento:", train_mse)

# Data de validación 
pred_test <- predict(modelo, newx = x_test)
test_mse_lasso <- mean((pred_test - y_test)^2)
paste("Error (mse) de test:", test_mse_lasso)

############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/


