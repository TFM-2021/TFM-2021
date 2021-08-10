
library(dplyr)

# Data selection

datos_aemet <- read.csv(file = './data/data_raw/AEMET/data_join_aemet.csv')


View(datos_aemet)

datos <- datos_aemet %>%
  select(c())





# Guardamos el dataset con las variables seleccionadas
  
  write.csv(datos, file = './02_Data_preparation/00_Select_data/00_aemet_variables.csv')


























