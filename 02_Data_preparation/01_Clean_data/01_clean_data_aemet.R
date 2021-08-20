
library(dplyr)

# Data cleaning

datos_aemet <- read.csv(file = './02_Data_preparation/00_Select_data/00_aemet_variables.csv')

datos_aemet <- datos_aemet[,-1]
summary(datos_aemet)


# Formato a cada columna: tipo fecha, character, numerico, etc.

attach(datos_aemet)


quitarparentesis <-  function(variable){
  substr(x,1,nchar(x) - 4)}
  
  
  
# Quitar los parentesis de algunas de las variables numericas

datos_aemet$p_max <- substr(datos_aemet$p_max,1,nchar(datos_aemet$p_max) - 4)
datos_aemet$q_max <- substr(datos_aemet$q_max,1,nchar(datos_aemet$q_max) - 4)
datos_aemet$ta_max <- substr(datos_aemet$ta_max,1,nchar(datos_aemet$ta_max) - 4)
datos_aemet$w_racha <- substr(datos_aemet$w_racha,1,nchar(datos_aemet$w_racha) - 4)

# Pasar las variables anteriores a tipo numérico

datos_aemet$p_max <- as.numeric(datos_aemet$p_max)
datos_aemet$q_max <- as.numeric(datos_aemet$q_max)
datos_aemet$ta_max <- as.numeric(datos_aemet$ta_max)

# datos_aemet$w_racha <- as.numeric(datos_aemet$w_racha)

# datos <- as.factor()
# 
# datos <- as.numeric()
# 
# datos <- as.Date()


# ¿Que hacemos con los nulos?

# La mitad de las variables tenian mas de la mitad de los datos que eran nulos






# Eliminarlos del dataset entero

datos <- datos_aemet %>%
  drop_na()








# Guardamos el csv con el data clean


write.csv(datos, file = './02_Data_preparation/01_Clean_data/aemet_clean.csv')






