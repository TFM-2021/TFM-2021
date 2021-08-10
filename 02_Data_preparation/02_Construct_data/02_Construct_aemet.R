library(dplyr)

# Datos

datos_aemet <- read.csv(file = './02_Data_preparation/01_Clean_data/aemet_clean.csv')






# Guardamos el dataset con las variables seleccionadas
write.csv(datos, file = './02_Data_preparation/02_Construct_data/aemet_clean_final.csv')
