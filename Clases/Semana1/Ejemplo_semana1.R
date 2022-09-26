############################################################################################################################/
##################### Ejemplos semana 1 ###################################################################
############################################################################################################################/
# Dudas: 
#José Daniel Conejeros (jdconejeros@uc.cl) 
############################################################################################################################/

# Librerías a utilizar
# install.packages("") # Sino lo tiene disponible
library(dplyr) # Manipulación 
library(tidyr) # Manipulación 
library(AER) # Contiene los datos 
library(estimatr) # Regresiones con errores estándar robustos
library(ggplot2) # Visualización de datos 

# Función que genera una tabla de balance
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
           p_val= round(t.test(unlist(`0`), unlist(`1`), alternative="two.sided",
                               var.equal = FALSE)[["p.value"]],4)
    ) %>% 
    select(-2,-3) %>% ungroup() 
}

## 1. Estudio experimental (STAR) ----

### 1.1 Datos ----

# Datos
data(STAR)
colnames(STAR)
glimpse(STAR)

# Tratamientos 
table(STAR$stark, useNA = "ifany") # Este es el que importa
table(STAR$star1, useNA = "ifany")
table(STAR$star2, useNA = "ifany")
table(STAR$star3, useNA = "ifany")

# Generamos una data de respaldo solo con las modificaciones a la data
df <- STAR %>% 
  tidyr::drop_na(stark, readk, mathk) %>% 
  mutate(str=if_else(stark=="small", 1, 0), 
         free_lunch=if_else(lunchk=="free", 1, 0), 
         male=if_else(gender=="male", 1, 0), 
         black=if_else(ethnicity=="afam", 1, 0)) %>% 
  rowwise() %>% 
  mutate(tscorek=(readk + mathk))

### 1.2. Balance + Efecto----

# Características antes del tratamiento 
names_var <- c("free_lunch", "male", "black")
# Generamos un objeto para guardar nuestros resultados
tabla1 <- tibble()
# Realizamos la iteración para el panel A 
for (i in rev(names_var)){
  tabla1 <- df %>% dif_means(x=i, y=str) %>% add_row(tabla1)
}

tabla1

# Tabla con el puntaje para tratados y controles
tabla2 <- df %>% 
  group_by(str) %>% 
  summarise(media=mean(tscorek, na.rm=TRUE),
            sd=sd(tscorek, na.rm=TRUE),
            n=n()) %>% 
  ungroup() 

tot <- as_tibble(cbind(str=NA_real_, 
      media=mean(df$tscorek, na.rm=TRUE),
      sd=sd(df$tscorek, na.rm=TRUE),
      n=nrow(df)))

tabla2 <- tabla2 %>% add_row(tot)
tabla2

# Efecto 
summary(lm(tscorek ~ str, data = df))

# Tamaño del efecto 
(tabla2[2,2] - tabla2[1,2])/tabla2[3,3]

### 1.3 Diferencia de medias ----

# Vemos la media para cada grupo
df %>% 
  group_by(str) %>% 
  summarise(n=n(),
            media=mean(tscorek, na.rm=TRUE), 
            sd=sd(tscorek, na.rm = TRUE))

# Aplicamos un test formal
t.test(tscorek ~ str, alternative = c("two.sided"),
       data=df,
       paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

### 1.4 Error estándar robusto ----
summary(lm_robust(tscorek ~ str, data = df, se_type = "stata")) 


## 2. Estudio observacional (CALIFORNIA) ----

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

### 2.2 Balance + Efecto)----

# Características entre grupos
names_var <- c("lunch", "english", "income")
# Generamos un objeto para guardar nuestros resultados
tabla3 <- tibble()
# Realizamos la iteración para el panel A 
for (i in rev(names_var)){
  tabla3 <- df2 %>% dif_means(x=i, y=small) %>% add_row(tabla3)
}

tabla3

# Tabla con el puntaje para tratados y controles
tabla4 <- df2 %>% 
  group_by(small) %>% 
  summarise(media=mean(tscorek, na.rm=TRUE),
            sd=sd(tscorek, na.rm=TRUE),
            n=n()) %>% 
  ungroup() 

tot <- as_tibble(cbind(small=NA_real_, 
                       media=mean(df2$tscorek, na.rm=TRUE),
                       sd=sd(df2$tscorek, na.rm=TRUE),
                       n=nrow(df2)))

tabla4 <- tabla4 %>% add_row(tot)
tabla4

# Efecto 
summary(lm(tscorek ~ small, data = df2))

# Tamaño del efecto 
(tabla4[2,2] - tabla4[1,2])/tabla4[3,3]

### 2.3 Error estándar robusto ----
summary(lm_robust(tscorek ~ small, data = df2, se_type = "stata")) 

### 2.4 Gráfico de asociación ----

ggplot(df2, aes(x = (students/teachers), y = tscorek)) +
  geom_point()+
  scale_y_continuous(limits = c(600, 720)) +
  scale_x_continuous(limits = c(10, 30)) +
  labs(y="Test score", x="Student-teacher ratio") +
  theme_classic()


## 3. Ejemplo tamaño de la clase ----

### 3.1 Gráfico de asociación ----

ggplot(df2, aes(x = ratio, y = tscorek)) +
  geom_point()+
  scale_y_continuous(limits = c(600, 720)) +
  scale_x_continuous(limits = c(10, 30)) +
  labs(y="Test score", x="Student-teacher ratio") +
  theme_classic()

# Generamos la etiqueta para la formula de regresión
intercepto <- round(coef(lm_robust(tscorek ~ ratio, data = df2, se_type = "stata"))[1], 1)
beta <- round(coef(lm_robust(tscorek ~ ratio, data = df2, se_type = "stata"))[2], 2)
lab <- paste("hat(TestScore)==",intercepto,beta,"*STR")

# Ploteamos
ggplot(df2, aes(x = ratio, y = tscorek)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="deepskyblue2") +
  scale_y_continuous(limits = c(600, 720)) +
  scale_x_continuous(limits = c(10, 30)) +
  labs(y="Test score", x="Student-teacher ratio") +
  annotate(geom="text", x=28, y=645, color="black",parse = TRUE, size=3,
           label=lab) +
  theme_classic()


### 3.2 Modelo ----
summary(lm_robust(tscorek ~ ratio, data = df2, se_type = "stata")) 


## 4. Brecha salarial por género ----

### 4.1 Datos ----

# Datos
data(wage1)
colnames(wage1)
glimpse(wage1)

### 4.2 Diferencia de medias ----

# Vemos la media para cada grupo
wage1 %>% 
  group_by(female) %>% 
  summarise(n=n(),
            media=mean(wage, na.rm=TRUE), 
            sd=sd(wage, na.rm = TRUE))

# Aplicamos un test formal
t.test(wage ~ female, alternative = c("two.sided"),
       data=wage1,
       paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

### 4.3 Regresión simple ----

# Modelo de regresión simple con SE robusto
# Ajustamos la categoría de referencia 
summary(lm_robust(wage ~ relevel(female, ref="Male"), data = wage1, se_type = "stata")) 

# Ajustamos la categoría de referencia por defecto 
summary(lm_robust(wage ~ female, data = wage1, se_type = "stata")) 


############################################################################################################################/
##################### FIN DEL EJEMPLO ###################################################################
############################################################################################################################/

