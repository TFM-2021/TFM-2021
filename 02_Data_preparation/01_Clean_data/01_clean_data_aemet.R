
library(dplyr)

# Data cleaning

datos_aemet <- read.csv(file = './02_Data_preparation/00_Select_data/00_aemet_variables.csv')

summary(datos_aemet)


# Formato a cada columna: tipo fecha, character, numerico, etc.

attach(datos_aemet)

# datos <- as.factor()
# 
# datos <- as.numeric()
# 
# datos <- as.Date()


# ¿Que hacemos con los nulos?

# Eliminarlos del dataset entero

datos <- datos_aemet %>%
  drop_na()








# Guardamos el csv con el data clean


write.csv(datos, file = './02_Data_preparation/01_Clean_data/aemet_clean.csv')






