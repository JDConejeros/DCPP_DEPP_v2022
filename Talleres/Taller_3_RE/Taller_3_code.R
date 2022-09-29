############################################################################################################################/
##################### Taller 3: Términos no lineales, Regresión con VPB y Experimentos #####################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Aplicación de términos no lineales e interacciones
# 2. Uso de modelos con variable dependiente binaria
# 3. Aplicación de experimentos 

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
               "car", "lmtest", "estimatr", "texreg", "labelled", "plotROC",
               "pROC", "ROCit", "InformationValue", "interactions", "sjPlot"))

sessionInfo()

devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)

############################################################################################################################/
# 1. Términos no lineales ----------------------------------------------------------------
############################################################################################################################/

exp <- import("Data/hh_98.dta")

exp <- exp %>% 
  mutate(lexptot = log(1 + exptot), # Logaritmo del total de gastos por hogar/año
         lnland = log((1 + hhland/100)), # Acres de tierra antes de unirse al programa
         agehead2 = agehead^2, # Efecto cuadrático de la edad
         dfmfd=factor(dfmfd, levels=c(0,1), labels=c("No tratado", "Tratdo"))
         )  

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

# Nuestras regresiones ya tienen un componente logaritmico tanto en Y como en X

m1 <- lm(lexptot ~ dfmfd, data=exp, weights=weight)
screenreg(m1)

m2 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead, data=exp, weights=weight)

m3 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess, data=exp, weights=weight)

m4 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
           pcirr + rice + wheat + milk + oil + egg, data=exp, weights=weight)

# Grafiquemos los resultados del modelo


# Efecto cuadrático 
m5 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
           pcirr + rice + wheat + milk + oil + egg + agehead2, data=exp, weights=weight)

interact_plot(m5, pred = agehead2, modx = dfmfd, plot.points = TRUE, linearity.check = TRUE)

# Efecto interacción
m6 <- lm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
           pcirr + rice + wheat + milk + oil + egg + agehead2 + educhead*dfmfd, data=exp, weights=weight)

interact_plot(m6, pred = educhead, modx = dfmfd, plot.points = TRUE, interval = TRUE, 
              x.label = "Años de educación", y.label = "Gasto por hogar",
              main.title = "Interacción",  legend.main = "Microcrédito:")

screenreg(l=list(m1, m2, m3, m5, m6), caption="Impacto del programa de microcréditos", float.pos="h" , digits = 3, 
          caption.above = TRUE,  include.ci = FALSE, stars = c(0.01, 0.05, 0.1), fsingle.row = T)

# Ref: https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html

############################################################################################################################/
# 2. Uso de variables dependientes binarias ----------------------------------------------------------------
############################################################################################################################/

issp <- import("Data/ISSP_2011_Salud.dta")  

glimpse(issp); str(issp); names(issp)

colnames(issp) <- tolower(colnames(issp))

############################################################################################################################/
## 2.1 Manipulación de la data ------------
############################################################################################################################/

# Filtremos los casos solo para chile
labelled::var_label(issp$v3) # Etiqueta de la variable
labelled::set_variable_labels(issp$v3) # ¿Cuál es el código de Chile?
table(issp$v3) # Tabla de frecuencia de la variable: ¿Cuántas observaciones son para el caso de Chile?
issp <- issp %>% filter(v3==152)
View(issp)

# (a) Generamos la variable dependiente principal: sedentarismo
labelled::set_variable_labels(issp$v57) # ¿Cómo podemos generar una variable binaria? 
table(issp$v57, useNA = "ifany") 

# ¿Podemos usar un as.numeric? 
issp <- issp %>% mutate(sedentario=if_else(v57==1, 1, 
                                           if_else(v57>=2 & v57<=5, 0, NA_real_)))

# Validamos nuestra variable nueva
table(issp$v57, issp$sedentario, useNA = "ifany") 

# (b) Proporción de personas sedentarias 
prop.table(table(issp$sedentario))
round(prop.table(table(issp$sedentario)),3)
round(prop.table(table(issp$sedentario)),3)*100
paste0(round(prop.table(table(issp$sedentario)),3)*100, "%")

# (c) Seleccionamos las variables de interés
# 1. Indice de masa corporal 
table(issp$v61) # Altura ¿Centímetros o metros? ¿Puede identificar algo extraño? 
table(issp$v62) # Peso   ¿Gramos o kilogramos? ¿Puede identificar algo extraño? 

# Debemos ajustar las variables antes estimar el índice de masa corporal
issp <- issp %>% mutate(altura=if_else(v61!=999, (v61/100), NA_real_)) 
table(issp$altura) # Altura en metros
summary(issp$altura)

issp <- issp %>% mutate(peso=if_else(v62!=999, v62, NA_real_)) 
table(issp$peso) # Altura en metros
summary(issp$peso)

# Realizamos el calculo para obtener la variable. 
issp <- issp %>% mutate(imc=peso/(altura^2))
summary(issp$imc)  # Validamos

# 2. Género 
labelled::set_variable_labels(issp$sex)
table(issp$sex, useNA = "ifany")
issp <- issp %>% mutate(mujer=as.numeric(sex==2))
table(issp$sex, issp$mujer, useNA = "ifany")  # Validamos

# 3. Edad 
summary(issp$age)
table(issp$age)
# Edad continua 
issp <- issp  %>% mutate(edad=as.numeric(age)) 
summary(issp$edad)  # Validamos
# Edad cualitativa: 3 categorías
issp <- issp %>% mutate(edad3=if_else(age<=35, 1, 
                                      if_else(age>35 & age<=60, 2, 
                                              if_else(age>60, 3, NA_real_))))
table(issp$age, issp$edad3, useNA = "ifany")  # Validamos
table(issp$edad3, issp$age, useNA = "ifany")  # Validamos
table(issp$edad3)

# 4. Años de educación 
issp <- issp  %>% mutate(educ=as.numeric(educyrs), 
                         educ=if_else(educ!=99, educ, NA_real_)) 

# 5. Fumador 
labelled::set_variable_labels(issp$v55)
table(issp$v55, useNA = "ifany")
issp <- issp %>% mutate(fumador=if_else(v55==1, 0, 
                                        if_else(v55>=2 & v55<=7, 1, NA_real_)))
table(issp$v55, issp$fumador, useNA = "ifany") # Validamos

# 6. Casado 
labelled::set_variable_labels(issp$marital)
table(issp$marital, useNA = "ifany")
issp <- issp %>% mutate(casado=if_else(marital==1, 1, 
                                       if_else(marital>=2 & marital<=7, 0, NA_real_)))
table(issp$marital, issp$casado, useNA = "ifany") # Validamos

# (d) Seleccionamos y limpiamos la base de datos 
colnames(issp)
issp <- issp %>% select(sedentario, imc, mujer, edad, edad3, educ, fumador, casado)
# Exploremos los casos pérdidos 
which(is.na(issp)) 
issp[is.na(issp),]
table(is.na(issp), useNA = "ifany") # Sabemos que hay 126 casos en el que a menos 1 variable es un missing
# Eliminamos los casos con missing: 1559-126=1433
issp <- na.omit(issp) # Base de datos final: 1438 obseravaciones. 126 observaciones perdidas.

############################################################################################################################/
## 2.2 MPL ------------
############################################################################################################################/

# (a) Estimamos el modelo simple
mpl1 <- lm(sedentario ~ imc, data=issp) # Estimo del modelo
summary(mpl1)                                          # Reporto sus resultados
screenreg(mpl1)                                        # Interpretamos

# (b) Estimamos el modelo múltiple
mpl2 <- lm(sedentario ~ imc + factor(mujer) + factor(edad3) + educ + factor(fumador) + factor(casado), data=issp) # Estimo del modelo
summary(mpl2)                                          # Reporto sus resultados
screenreg(mpl2)                                        # Interpretamos

issp$pred <- predict(mpl2)
View(issp)

############################################################################################################################/
## 2.3 MPB ------------
############################################################################################################################/

# (a) Estimamos el modelo múltiple
mpb1 <- glm(factor(sedentario) ~ imc + factor(mujer) + factor(edad3) + educ + factor(fumador) + factor(casado), 
            family = binomial(link="probit"), data = issp)
summary(mpb1)                                          # Reporto sus resultados
screenreg(mpb1)                                        # Interpretamos
# Comparemos con el modelo anterior 
screenreg(mpl2)    

############################################################################################################################/
## 2.4 MLB ------------
############################################################################################################################/

# (a) Estimamos el modelo múltiple
mlb1 <- glm(factor(sedentario) ~ imc + factor(mujer) + factor(edad3) + educ + factor(fumador) + factor(casado), 
            family = binomial(link="logit"), data = issp)
summary(mlb1)                                          # Reporto sus resultados
screenreg(mlb1)                                       # Interpretamos
# Comparemos con el modelo anterior 
screenreg(mpl2)
screenreg(mpb1)

# (b) Interpretamos el beta de la educación 
mlb1[["coefficients"]][["educ"]] # Logaritmo de las odds
exp(mlb1[["coefficients"]][["educ"]]) # Razón de odds
1 -  exp(mlb1[["coefficients"]][["educ"]]) 
# Las chances de ser sedentario disminuyen en 0.036 por cada año de educación.
# En otras palabras la probabilidad de ser sedentario disminuye en 3.64% por cada año adicional de educación,
round(100*(1-exp(mlb1[["coefficients"]][["educ"]])),2)

# Obtener todo el modelo en odds
exp(cbind(OR=coef(mlb1), confint(mlb1))) # Intervalos a un 95%

# (c) Evaluemos la capacidad predictiva del modelo 
screenreg(mlb1)
pred <- predict(mlb1, type = "response") # Obtenemos las predicciones del modelo
# Graficamos la curva roc: nos indica el poder de clasificación del modelo. 
graph <- ggplot(issp, aes(d = issp$sedentario, m = pred)) + geom_roc(linetype = 3)
graph  + style_roc(theme = theme_light) +
  theme(axis.text = element_text(colour = "black")) +
  geom_abline(slope = 1, colour="red") +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensivity", breaks = seq(0, 1, by = .1)) +
  annotate("text", x = .35, y = .6, colour = "black", size = 5, fontface = "bold",
           label = paste("AUC =", round(calc_auc(graph)$AUC, 2))) + 
  theme_light()

# Otra manera (paquete Rocit): 
plotROC(issp$sedentario, pred, returnSensitivityMat  = TRUE)

# Sensibilidad: clasificar a los verdaderos positivos. 
# Especificidad: clasificar a los negativos negativos
opt <- optimalCutoff(issp$sedentario, pred) # Óptimo punto de corte para el modelo 
confusionMatrix(issp$sedentario, pred, opt) # Evalúo la predicción del modelo.

# (e) Comparemos los tres modelos:
info <- data.frame(imc=rep(25,3), mujer=rep("1",3), edad3=as.factor(c("1", "2", "3")), educ=rep(mean(issp$educ),3), fumador=rep("0",3), casado=rep("0",3))

predict(mpl2, info, type="response")
predict(mpb1, info, type="response")
predict(mlb1, info, type="response")

############################################################################################################################/
# 3. Aplicación de experimentos ----------------------------------------------------------------
############################################################################################################################/

rm(list=(ls()))

# Abrimos las bases de datos
data <- import("Data/ms_blel_jpal_long.dta")      # BBDD en formato long 
data_wide <- import("Data/ms_blel_jpal_wide.dta") # BBDD en formato wide
attendance <- haven::read_dta("Data/ms_ei.dta")   # BBDD de donde extraemos los días de asistencia (tabla 9)

# Para revisar etiquetas
sjPlot::view_df(data)
sjPlot::view_df(data_wide)
sjPlot::view_df(attendance)

# ¿Qué diferencias puede observar a partir de las BBDD? 
glimpse(data); str(data); names(data)
glimpse(data_wide); str(data_wide); names(data_wide)
glimpse(attendance); str(attendance); names(attendance)

############################################################################################################################/
## 3.1 Tabla 1 del artículo  ------------
############################################################################################################################/

# Filtramos nuestra BBDD con la info de cada panel
baseline <- data %>% filter(round==1)
endline <- baseline %>% filter(in_r2==1) # ¿En qué consiste este filtro en específico?
# Función que va a estimar todas las estadísticas de interés (la definimos)
dif_means <- function(x, y, data){
  data %>% 
    select({{ x }}, {{ y }}) %>%                              
    drop_na() %>%                                             
    pivot_longer(names_to = "variable", !{{ y }}) %>% 
    group_by({{ y }}, variable) %>% 
    summarise(value = list(value)) %>% 
    pivot_wider(names_from = {{ y }}) %>% 
    group_by(variable) %>% 
    mutate(m_t = round(t.test(unlist(`0`), unlist(`1`), alternative="two.sided", 
                              var.equal = FALSE)$estimate[2],4),
           m_c = round(t.test(unlist(`0`), unlist(`1`), alternative="two.sided", 
                              var.equal = FALSE)$estimate[1],4),
           dif = round((m_t-m_c),3),
           se  = round(t.test(unlist(`0`), unlist(`1`), alternative="two.sided", 
                              var.equal = FALSE)[["stderr"]],4),
           n_t=length(unlist(`1`)),
           n_c=length(unlist(`0`))) %>% 
    select(-2,-3) %>% ungroup() 
}

# Realizamos un proceso iterativo para cada variable

# Un vector con el nombre de las variables
names_var <- c("st_female1", "st_age1")
# Generamos un objeto para guardar nuestros resultados
tabla_a <- tibble()
# Realizamos la iteración para el panel A 
for (i in rev(names_var)){
  tabla_a <- baseline %>% dif_means(x=i, y=treat) %>% add_row(tabla_a)
}

# Realizamos la iteración para el panel B: ¿Qué difefencia hay? 
names_var2 <- names_var[1:2] #Ajustamos el nombre de las variables
tabla_b <- tibble()
for (i in rev(names_var2)){
  tabla_b <- endline %>% dif_means(x=i, y=treat) %>% add_row(tabla_b)
}

# Guardamos nuestros resultados
tabla1 <- tabla_a %>% add_row(tabla_b) 
# Ajustamos nuestra tabla
tabla1[,c(2:3)] <- round(tabla1[,c(2:3)], digits = 2)
tabla1[,c(4:5)] <- round(tabla1[,c(4:5)], digits = 3)
tabla1 <- tabla1 %>% mutate(variable=case_when(variable=="st_female1"~"Mujer",
                                               variable=="st_age1"~"Edad (años)",
                                               TRUE ~ variable))

# Reportamos nuestra tabla de resultados: 
kable(tabla1, booktabs=TRUE, align=c("lcccccc"), linesep="", format = "html",
      caption="Tabla 1 - Descriptivos de la muestra y balance en observables",           
      col.names=c("", "Media (Tratados)", "Media (Controles)","Diferencia", "Error Estándar", 
                  "Observaciones (Tratados)","Observaciones (Controles)"))%>% 
  column_spec(column = c(2:3), width = "0.75in") %>% 
  column_spec(column = c(5:7), width = "0.8in") %>% 
  pack_rows("Panel A. Todos los estudiantes en la muestra de línea base", 1, 2) %>%
  pack_rows("Características demográficas", 1, 2, bold = FALSE) %>%
  pack_rows("Panel B. Solo estudiantes presentes al final", 3, 4, latex_gap_space = "1em")%>%
  pack_rows("Características demográficas", 3, 4, bold = FALSE)  %>%
  kable_styling(latex_options=c("hold_position"), full_width = F) %>%      
  kableExtra::footnote(general="Elaboración propia a partir de los datos del estudio.", 
                       footnote_as_chunk=T, general_title="Fuente:", fixed_small_size=T)

############################################################################################################################/
## 3.2 Figura 2 del artículo  ------------
############################################################################################################################/

# Vamos a filtrar la data de interés
endline2 <- data %>% filter(round==2)
# Vamos a generar una función para guardar la información
means <- function(x, y, data){
  tf1 <- data %>% 
    group_by({{ x }}) %>% 
    summarise(mean=round(mean({{ y }}, na.rm=TRUE),2),
              sd=round(sd({{ y }}, na.rm=TRUE),2), 
              n=n()) %>% 
    mutate(ee=sd/sqrt(n),
           izq=round(mean-ee*qnorm(.025,lower.tail=FALSE),2),
           der=round(mean+ee*qnorm(.025,lower.tail=FALSE),2))
  return(tf1)
}
# Panel A Matemáticas
A <- rbind(means(x=treat, y=m_theta_mle, data=baseline),
           means(x=treat, y=m_theta_mle, data=endline2))
A <- cbind(tipo=c(rep("Baseline", 2), rep("Endline", 2)), A)
# Generamos el gráfico para el panel
panela <- A %>% ggplot(aes(y=mean, x=tipo, fill=as.factor(treat))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=izq, ymax=der), width=0.1, size=0.5 ,color="gray60", 
                position=position_dodge(0.93), alpha=0.9) +
  geom_text(aes(label=mean), vjust=-0.7, size=4, position = position_dodge(0.93)) +
  scale_y_continuous(limits=c(-0.2,0.85), n.breaks=8) +
  scale_fill_manual(values=c("gray80","dodgerblue4"),
                    name = " ", labels = c("Control", "Tratamiento")) +
  labs(title="Panel A. Matemáticas", y="Puntaje normalizado", x=" ") +
  theme_classic() +
  theme(panel.grid.major.y =  element_line(colour = "gray90"),
        legend.background = element_rect(colour = "black", size = 0.5, linetype="solid"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))
panela

# Unimos el gráfico (código para agregar el panel B)
ggarrange(panela, ncol=2, nrow=1,  common.legend = TRUE, legend="bottom") 


############################################################################################################################/
## 3.3 Tabla 2: ITT ------------
############################################################################################################################/

# Modelos con efectos fijos
m1 <- lm_robust(m_theta_mle2 ~ treat + m_theta_mle1, data = data_wide, se_type = "HC1", 
                fixed_effects = ~ factor(strata))
# Modelos sin efectos fijos
m3 <- lm_robust(m_theta_mle2 ~ treat + m_theta_mle1, data = data_wide, se_type = "HC1")

# Presentamos en una tabla 
screenreg(l=list(m1, m3), include.ci = FALSE,  digits = 3, 
          caption="Tabla 2 - Efectos por intención de tratar (ITT) en un marco de regresión ", 
          custom.header = list("IRT score-estandarizado (endline) \\vspace{0.2 cm}" = 1:2),
          custom.model.names = (c("Math (1)", "Math (3) \\vspace{0.2 cm}")),
          custom.coef.names=c("Tratamiento","Puntaje de línea base", "Intercepto"), 
          custom.gof.rows = list("Efectos Fijos" = c("Sí", "No")), 
          include.adjrs = FALSE, include.rmse=FALSE, float.pos="h", single.row = FALSE, 
          fsingle.row = TRUE, caption.above = TRUE, stars = c(0.01, 0.05, 0.1), fontsize="small",
          custom.note = ("\\parbox{.3\\linewidth}{\\vspace{4pt}%stars. 
                      Error estándar robusto entre paréntesis.}"))  

# Exporar la tabla
htmlreg(l=list(m1, m3), single.row = TRUE,  
        include.ci = FALSE,  digits = 4,
        stars = c(0.001, 0.01, 0.05), file="Modelo1.xls") 

############################################################################################################################/
## 3.4 Tabla 4: Efectos Heterogéneos -----
############################################################################################################################/

data_wide <- data_wide %>% 
  mutate(treat_st_female1=treat*st_female1)
# Estimamos los modelos:
# Female
mf1 <- lm_robust(m_theta_mle2 ~ treat + st_female1 + treat_st_female1 + m_theta_mle1, 
                 data = data_wide, se_type = "HC1", fixed_effects = ~ strata, alpha = 0.05) 
mf2 <- lm_robust(h_theta_mle2 ~ treat + st_female1 + treat_st_female1 + h_theta_mle1, 
                 data = data_wide, se_type = "HC1", fixed_effects = ~ strata, alpha = 0.05) 
# Reportamoms
screenreg(l=list(mf1, mf2), include.ci = FALSE,  digits = 3, 
          caption="Tabla 4 - Efectos heterogéneos (género)", 
          custom.header = list("Female" = 1:2), 
          custom.model.names = (c("\\vspace{0.2 cm}Math (1)", 
                                  "Hindi (2) \\vspace{0.2 cm}")),
          omit.coef = c('m_theta_mle1|h_theta_mle1'),
          custom.coef.names=c("Tratamiento", "Covariable", "Interacción"), 
          include.adjrs = FALSE, include.rmse=FALSE, float.pos="h", single.row = FALSE, 
          fsingle.row = TRUE, caption.above = TRUE, stars = c(0.01, 0.05, 0.1), fontsize="small",
          custom.note = ("\\parbox{.3\\linewidth}{\\vspace{4pt}%stars. \\\\
       Error estándar robusto entre paréntesis. Todas las regresiones
       incluyen efectos fijos por estratos y son controladas por el 
       puntaje de línea base.}"))  

# Exporar la tabla
htmlreg(l=list(mf1, mf2), single.row = TRUE,  
        include.ci = FALSE,  digits = 4,
        stars = c(0.001, 0.01, 0.05), file="Modelo2.xls") 

############################################################################################################################/
## 3.4 Tabla 9: Uso de variables instrumentales -----
############################################################################################################################/

# Aplicamos variables instrumentales cuando Cov(D,ε) ̸= 0 por lo que D 
# es endogeno y estimadores OLS son sesgado e inconsistente.

# Esto puede ocurrir por distintas razones: variable omitida, error de medición, sesgo de selección, etc. 

# La IV es la de encontrar una variable que este correlacionada con C (la parte exógena de D) pero no correlacionada con el error.
# Restricción de exclusión 
# Condición de instrumento

# El instrumento afecta el resultado solo a través del tratamiento. No de forma directa. 

# Estima el efecto solo para los compliers

# Pista: pensar la idea de los compliers. 

# Puede contemplar la identificación de un efecto causal usando IV solo si puede defender teórica y lógicamente la restricción de exclusión, ya que la restricción de exclusión es una suposición no comprobable.

# Agregamos los datos 
# Unimos con un join
data_t9 <- data_wide %>% full_join(attendance, by="st_id")
# Reemplazamos los NA 
data_t9 <- data_t9 %>% mutate(att_tot2=ifelse(is.na(att_tot), 0, att_tot)) 
# Modelos IV
miv1 <- estimatr::iv_robust(m_theta_mle2 ~ att_tot2 + m_theta_mle1 | treat + m_theta_mle1, 
                            data = data_t9, se_type = "HC1", fixed_effects = ~ strata)
# Reportamos
screenreg(l=list(miv1), include.ci = FALSE,  digits = 4, 
          caption="Tabla 9 - Respuesta la dosis de asistencia", 
          custom.header = list("Estimaciones IV \\vspace{0.2 cm}" = 1), 
          custom.model.names = (c("Math (1)\\vspace{0.2 cm}")),
          custom.coef.names=c("Asistencia (días)", "Puntaje de línea base"), 
          include.adjrs = FALSE, include.rmse=FALSE, float.pos="h", single.row = FALSE, 
          fsingle.row = TRUE, caption.above = TRUE, stars = c(0.01, 0.05, 0.1), fontsize="small",
          custom.note = ("\\parbox{.3\\linewidth}{\\vspace{4pt}%stars. \\\\
       Error estándar robusto entre paréntesis. Todas las regresiones
       incluyen efectos fijos por estratos.}"))    

# Exporar la tabla
htmlreg(l=list(miv1), single.row = TRUE,  
        include.ci = FALSE,  digits = 4,
        stars = c(0.001, 0.01, 0.05), file="Modelo3.xls") 


############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/
