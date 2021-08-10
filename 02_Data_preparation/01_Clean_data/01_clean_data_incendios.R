
library(dplyr)

# Data cleaning

datos_incendios <- read.csv(file = './02_Data_preparation/00_Select_data/00_incendios_variables.csv')

datos <- datos_incendios[,-1] # Qutamos la columna X

# Formato a cada columna: tipo fecha, character, numerico, etc.


datos$idcomunidad <- as.character(datos$idcomunidad)
datos$idprovincia <- as.character(datos$idprovincia)
datos$idmunicipio <- as.character(datos$idmunicipio)

# datos$lat <- as.character(datos$lat)
# datos$lng <- as.character(datos$lng)


datos$fecha <- as.Date(datos_incendios$fecha)

summary(datos) 

# ¿Que hacemos con los nulos?

# Eliminarlos del dataset entero

# datos <- datos_incendios %>%
#   drop_na()


# Eliminarlos de algunos de los campos


# En el caso de latitud y longitud ya que coincidian en las mismas observaciones

datos <- datos_incendios %>%
   drop_na(lat, lng)


# Sustituir los nulos por 0


# En el caso de los campos de muertos y heridos se sustituyen los NaN por 0
# Ya que a partir de la informacion historica que tenemos en la mayoría de los
# incendios que se producen, no se hayan muertos ni heridos.

datos <- mutate_at(datos, c("muertos", "heridos"), ~replace(., is.na(.), 0))

summary(datos)


# Guardamos el csv con el data clean


write.csv(datos, file = './02_Data_preparation/01_Clean_data/incendios_clean.csv')







