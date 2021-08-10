

library(dplyr)


# Data selection

datos_incendios <- read.csv(file = './data/data_raw/incendios.csv')

View(datos_incendios)

summary(datos_incendios)


# Las variables que no nos interesan son: id, el nombre del municipio 
# (porque tenemos el id provincia y id comunidad autonoma), latlng_explicit,
# causa supuesto porque siempre es 1. , personal y medios porque nos dan igual, 




datos <- datos_incendios %>%
  select(c(2:5, 7:9, 11, 14:17, 20:21))

# idmunicipio?


# Guardamos el dataset con las variables seleccionadas
write.csv(datos, file = './02_Data_preparation/00_Select_data/00_incendios_variables.csv')



















