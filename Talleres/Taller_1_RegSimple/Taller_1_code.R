############################################################################################################################/
##################### Taller 1: Regresión Simple ###############################################################################
############################################################################################################################/
# Datos para la evaluación de impacto en políticas públicas
# Autores: José Daniel Conejeros (jdconejeros@uc.cl) & Pablo Celhay (pacelhay@uc.cl)
############################################################################################################################/
# Los objetivos del taller son los siguientes

# 1. Trabajar en el procesamiento de múltiples bases de datos
# 2. Exploración bases de datos y generación de variables de interés
# 3. Análisis de regresión simple y sesgo por variable omitida
# 4. Introducción Rmarkdown

############################################################################################################################/
# 0. Ajustes iniciales  ----------------------------------------------------------------
############################################################################################################################/

# Ajustes de arranque 
options(scipen=999) # Desactiva la notación científica
options(max.print = 99999999) # Max print de la consola
rm(list=(ls()))   # limpia el enviroment (dispongo de más memoria temporal)

# Vamos a cargar algunas librerías que vamos a utilizar
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

install_load(c("rio", "dplyr",  "stringr", "tidyr",  "janitor", 
               "ggplot2","kableExtra","texreg", "Publish"))

sessionInfo()

############################################################################################################################/
# 1. Cargamos y exploramos nuestra BBDD principal ----------------------------------------------------------------
############################################################################################################################/

# Muestra 50000 obs. PSU 2007, 80% menores ingreso FUAS. Puntaje promedio entre lenguaje y matemáticas
psu08 <- import("data/PSU_promlm_2008.dta")
colnames(psu08)
glimpse(psu08)

sum <- function(data, x){
  data %>%
    dplyr::summarize(count=length(na.omit({{ x }})),
                     media=mean({{ x }}, na.rm=TRUE),
                     sd=sd({{ x }}, na.rm=TRUE),
                     min=min({{ x }}, na.rm=TRUE),
                     q25=quantile({{ x }}, na.rm=TRUE, probs=0.25),
                     q50=quantile({{ x }}, na.rm=TRUE, probs=0.50),
                     q75=quantile({{ x }}, na.rm=TRUE, probs=0.75),
                     max=max({{ x }}, na.rm=TRUE))
}

# Veamos una tabla de descriptivos
tabla1 <- psu08 %>% sum(promlm) %>% mutate(var="PSU") %>% select(9, 1:8)
tabla1


kable(tabla1, format="html", booktabs=TRUE, linesep="") %>%
  kable_styling(latex_options=c("hold_position"), full_width = F)

# Veamos una figura
f1 <- psu08 %>%
  ggplot(aes(x=promlm)) +
  geom_histogram(aes(y = ..density..), fill="#7FC5DC", color="#7FC5DC", alpha=0.5, position = "identity", bins = 100) +
  geom_density(color="blue", alpha=0.8) +
  geom_vline(aes(xintercept = mean(promlm)), color = "black", linetype = "dashed", size = 0.75) +
  geom_vline(xintercept = 475, color = "red", linetype = "dashed", size = 0.75) +
  labs(title="Histograma puntajes PSU 2008",
       y="Densidad", x="Puntajes") +
  theme_light()

f1

############################################################################################################################/
# 2. Generemos una variable de tratamiento ----------------------------------------------------------------
############################################################################################################################/

psu08 <- psu08 %>% 
  mutate(treat=if_else(promlm>=475, 1, 0))

psu08 <- psu08 %>% 
  mutate(no_treat=1-treat)

head(psu08)

############################################################################################################################/
## Ejercicio 1 ----
############################################################################################################################/

# a. Explore descriptivamente ambos grupos generados

# b. ¿Qué representan los 0? ¿Qué? representan los 1?

# c. ¿Es estadísticamente distinto el resultado entre el grupo de tratados y controles?

############################################################################################################################/
# 3. Agreguemos una variable de resultado y algunas covariables ----------------------------------------------------------------
############################################################################################################################/

# a. Carguemos las bases de datos de matricula de la educación superior

datas <- list.files("data/Matricula_ES", pattern = "*.csv")
for(i in datas) { 
  name <- paste0("mat_", stringr::str_extract(i, pattern = "20[0-1][0-9]")) # Nombre para cada objeto
  data <- import(paste0("data/Matricula_ES/", i))               # Importamos
  assign(name, data) # Asignamos
}

rm(list=ls()[! ls() %in% c("psu08", "mat_2008", "mat_2009")])

##################################################/
# b. Exploremos las bases de datos 
glimpse(mat_2008)
glimpse(mat_2009)

# Limpieza de nombres
mat_2008 <- mat_2008 %>%     
  janitor::clean_names()  

mat_2009 <- mat_2009 %>%     
  janitor::clean_names()  

# Función que nos haga explorar toda la base de datos
func_explore <- function(base, x){
  # Base=data utilizada
  # x = variable de interés
  if(class(base[,x])=="character" | class(base[,x])=="logical"){
    # Nombre de la variable 
    print(paste0("Variable: ", colnames(base[x])))
    
    # Tipo de variable
    print(paste0("Tipo de variable: ", class(base[,x])))  
    
    # Atributos
    print("Atributos de la variable:")
    print(sort(unique(base[,x])))                             
    
    # Tabla de frecuencia
    print("Tabla de frecuencia:")
    print(table(base[,x], useNA = "ifany"))              
    
    # Conteo de casos perdidos 
    paste0("Casos perdidos:", as.numeric(base::sum(is.na(base[,x])))) 
  }
  
  else{
    # Nombre de la variable 
    print(paste0("Variable: ", colnames(base[x])))
    
    # Tipo de variable
    print(paste0("Tipo de variable: ", class(base[,x])))  
    
    # Atributos
    print("Atributos de la variable:")
    print(sort(unique(base[,x])))                            
    
    # Descriptivos
    print("Descriptivos:")
    print(paste0("Media:", mean(base[,x], na.rm=TRUE)))
    print(paste0("SD:", sd(base[,x], na.rm=TRUE)))
    print(paste0("Min:", min(base[,x], na.rm=TRUE)))
    print(paste0("Q25:", quantile(base[,x], na.rm=TRUE, probs=0.25)))
    print(paste0("Mediana:", quantile(base[,x], na.rm=TRUE, probs=0.50)))
    print(paste0("Q75:", quantile(base[,x], na.rm=TRUE, probs=0.75)))
    print(paste0("Max:", max(base[,x], na.rm=TRUE)))
    
    # Conteo de casos perdidos 
    paste0("Casos perdidos:", as.numeric(base::sum(is.na(base[,x]))))
  }
}

glimpse(mat_2008)

names <- colnames(mat_2008) 

for(j in names[c(4,6,7)]){
  print(func_explore(base=mat_2008, x=j))
  cat(sep="\n\n")
} 

table(duplicated(mat_2008$mrun))

##################################################/
# c. Realizamos otro loop para procesar las datas
unique(mat_2008$nivel_global)
unique(mat_2008$tipo_inst_1)
unique(mat_2008$gen_alu)

names_data <- ls(pattern = "mat_*")
for(i in names_data) {
  data <- get(i)  # Cargamos la data 
  
  # Procesamiento de cada base de datos
  data <- data %>% 
    filter(!is.na(mrun) & nivel_global %in% c("Pregrado", "PREGRADO")) %>% 
    mutate(mat_univ = if_else(tipo_inst_1 %in% c("Universidades", "UNIVERSIDADES"), 1, 0),
           mat_ipcft = if_else(tipo_inst_1 %in% c("Institutos Profesionales", "INSTITUTOS PROFESIONALES", 
                                                  "Centros de Formación Técnica", "CENTROS DE FORMACIÓN TÉCNICA"), 1, 0),
           mat_alguno= if_else(mat_univ==1 | mat_ipcft==1, 1, 0),
           genero=gen_alu-1) %>% 
    select(mrun, genero, valor_arancel, mat_univ, mat_ipcft, mat_alguno) 
  
  assign(i, data)
  
}

table(mat_2008$mat_alguno)
table(mat_2008$mat_univ)
table(mat_2008$mat_ipcft)

##################################################/
# d. Vamos a unir todo en una gran base de datos

matriculas_es <- data.frame()
for(i in names_data) {
  # Cargamos la data 
  data <- get(i) 
  
  # Unimos la data 
  matriculas_es <- rbind(matriculas_es, data)
}
nrow(matriculas_es)

##################################################/
# e. Ahora vamos a agrupar nuestro resultados por mrun
table(duplicated(matriculas_es$mrun))

dups <- matriculas_es %>% 
  filter(duplicated(mrun) | duplicated(mrun, fromLast = TRUE)) %>% 
  arrange(mrun) 

# Por simplicidad del análisis vamos a descartar opciones

matriculas <- matriculas_es %>% 
  filter(!duplicated(mrun)) 

names <- colnames(mat_2008)
names

for(i in names[c(-1)]) {
  print(i)
  print(table(matriculas[,i], useNA = "ifany")) 
}

##################################################/
# f. Realizamos el match con nuestra base PSU
data <- psu08 %>% 
  left_join(matriculas, by="mrun")  %>%
  mutate_all(~(replace_na(.,0)))

save(data, file="Output/data_cae.Rdata")

############################################################################################################################/
## Ejercicio 2 ----
############################################################################################################################/

# a. ¿Qué otra variable de la base de datos de los matriculados podría ser importante? Agreguela a su análisis. 
rm(list=ls()[! ls() %in% c("psu08")])
datas <- list.files("data/Matricula_ES", pattern = "*.csv")
for(i in datas) { 
  name <- paste0("mat_", stringr::str_extract(i, pattern = "20[0-1][0-9]")) # Nombre para cada objeto
  data <- import(paste0("data/Matricula_ES/", i))               # Importamos
  assign(name, data) # Asignamos
}

# Limpieza de nombres
mat_2008 <- mat_2008 %>%     
  janitor::clean_names()  

mat_2009 <- mat_2009 %>%     
  janitor::clean_names()  

names_data <- ls(pattern = "mat_*")
for(i in names_data) {
  data <- get(i)  # Cargamos la data 
  
  # Procesamiento de cada base de datos
  data <- data %>% 
    filter(!is.na(mrun) & nivel_global %in% c("Pregrado", "PREGRADO")) %>% 
    mutate(mat_univ = if_else(tipo_inst_1 %in% c("Universidades", "UNIVERSIDADES"), 1, 0),
           mat_ipcft = if_else(tipo_inst_1 %in% c("Institutos Profesionales", "INSTITUTOS PROFESIONALES", 
                                                  "Centros de Formación Técnica", "CENTROS DE FORMACIÓN TÉCNICA"), 1, 0),
           mat_alguno= if_else(mat_univ==1 | mat_ipcft==1, 1, 0),
           genero=gen_alu-1, 
           valor_arancel=scale(valor_arancel)
           ) %>% 
    select(mrun, genero, mat_univ, mat_ipcft, mat_alguno, valor_arancel) 
  
  assign(i, data)
  
}

matriculas_es <- data.frame()
for(i in names_data) {
  # Cargamos la data 
  data <- get(i) 
  
  # Unimos la data 
  matriculas_es <- rbind(matriculas_es, data)
}
nrow(matriculas_es)


matriculas <- matriculas_es %>% 
  filter(!duplicated(mrun)) 

data <- psu08 %>% 
  left_join(matriculas, by="mrun")  %>%
  mutate_all(funs(replace_na(.,0)))

# b. ¿Cuál es el % de matriculados para cada nivel (universidad, ipcft o alguno de los dos)? 
names <- colnames(data)
names

for(i in names[c(6:8)]) {
  print(i)
  print(prop.table(table(data[,i], useNA = "ifany"))) 
}

# c. ¿Hay diferencia entre personas que recibieron y no recibieron el CAE en las variables observadas que tiene disponible? Explore con tablas y/o gr?ficos.
aggregate(genero ~ treat, data=data, FUN="mean")   
aggregate(valor_arancel ~ treat, data=data, FUN="mean")   


# d. En su carpeta del taller tiene una serie de bases de datos con información para las personas tituladas. Extraiga esa información y agreguela a su base de datos. ¿Qué podría ser relevante? 
datas <- list.files("data/Titulados_ES", pattern = "*.csv")
for(i in datas) { 
  name <- paste0("tit_", stringr::str_extract(i, pattern = "20[0-1][0-9]")) # Nombre para cada objeto
  base <- import(paste0("data/Titulados_ES/", i))               # Importamos
  assign(name, base) # Asignamos
}

# e. Genere una variable que indique la probabilidad de titulación de la carrera.
names_data <- ls(pattern = "tit_*")
for(i in names_data) {
  data <- get(i)  # Cargamos la data 
  
  # Procesamiento de cada base de datos
  data <- data %>% 
    janitor::clean_names() %>% 
    filter(!is.na(mrun) & nivel_global %in% c("Pregrado", "PREGRADO")) %>% 
    mutate(tit_univ = if_else(tipo_inst_1 %in% c("Universidades", "UNIVERSIDADES"), 1, 0),
           tit_ipcft = if_else(tipo_inst_1 %in% c("Institutos Profesionales", "INSTITUTOS PROFESIONALES", 
                                                  "Centros de Formación Técnica", "CENTROS DE FORMACIÓN TÉCNICA"), 1, 0),
           tit_alguno= if_else(tit_univ==1 | tit_ipcft==1, 1, 0)) %>% 
    select(mrun, tit_univ, tit_ipcft, tit_alguno) 
  
  assign(i, data)
  
}

titulados_es <- data.frame()
for(i in names_data) {
  # Cargamos la data 
  base <- get(i) 
  
  # Unimos la data 
  titulados_es <- rbind(titulados_es, data)
}
nrow(titulados_es)


titulados <- titulados_es %>% 
  filter(!duplicated(mrun)) 

load("Output/data_cae.Rdata")

data <- data %>% 
  left_join(titulados, by="mrun")  

# f. Guarde sus resultados
save(data, file="Output/data_cae.Rdata")

############################################################################################################################/
# 3. Regresión bivariada ----------------------------------------------------------------
############################################################################################################################/
rm(list=ls()[! ls() %in% c("data")])

# Vamos estimar nuestros modelos de regresión y reportar todo en una tabla

# lm(Y ~ X + Y + Z, data=BD)
?lm

# Modemos estimar nuestros modelos sencillamente
modelo <- lm(mat_univ ~ treat, data=data)

# Presentar el modelo. ¿Qué tipo de modelo estamos observando? 
summary(modelo)

#library(texreg)
screenreg(l=list(modelo))

# Agreguemos un segundo y tercer modelo 
modelo_univ <- lm(mat_univ ~ treat, data=data)
modelo_ipcft <- lm(mat_ipcft ~ treat, data=data)
modelo_es <- lm(mat_alguno ~ treat, data=data)

screenreg(l=list(modelo_univ, modelo_ipcft, modelo_es))

# ¿Cómo interpretamos estos coeficientes?

# ¿Podríamos decir que este es el efecto de CAE sobre la probabilidad de matricularse en la universidad/ip/educación superior?

############################################################################################################################/
# 4. Sesgo por variable omitida ----------------------------------------------------------------
############################################################################################################################/

# Distribución del género entre personas con el CAE y sin el CAE
aggregate(genero ~ treat, data=data, FUN="mean")   # Media 
aggregate(genero ~ treat, data=data, FUN="var")    # Varianza
var.test(genero ~ treat, alternative="two.sided", data=data) # Test de varianza
t.test(genero ~ factor(treat), alternative="two.sided", var.equal = TRUE, data=data) # Test de media
ci.mean(genero ~ treat, alpha = 0.05, normal = FALSE, data=data) #Intervalos    

prop.table(table(data$treat, data$genero),  margin=2)
prop.test(table(data$treat, -data$genero), alternative = "two.sided", conf.level = 0.95) # Test de proporciones
chisq.test(data$treat, -data$genero, correct = TRUE)

# Condiciones para evaluar el sesgo:
m_sesgo <- lm(treat ~ genero, data=data)
summary(m_sesgo)

# Modelos de regresión múltiple ajustados por el desbalance
modelo_univ2 <- lm(mat_univ ~ treat + factor(genero), data=data)
modelo_ipcft2 <- lm(mat_ipcft ~ treat + factor(genero), data=data)
modelo_es2 <- lm(mat_alguno ~ treat + factor(genero), data=data)

screenreg(l=list(modelo_univ, modelo_univ2,
                 modelo_ipcft, modelo_ipcft2,
                 modelo_es, modelo_es2), digits = 4)


############################################################################################################################/
## Ejercicio 3 ----
############################################################################################################################/

# a. Estime un modelo incorporando como variable dependiente la probabilidad de titulación. ¿Qué le dicen sus resultados?

# b. ¿Qué otras variables podemos estar omitiendo en el análisis? ¿Tiene alguna disponible en sus bases de datos? Evalue empíricamente.

# c. Presente un modelo de regresión ajustado.

############################################################################################################################/
########################### FIN DEL TALLER ###############################################################################
############################################################################################################################/
