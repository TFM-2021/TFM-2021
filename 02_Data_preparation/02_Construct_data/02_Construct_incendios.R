library(dplyr)

# Datos

datos_incendios <- read.csv(file = './02_Data_preparation/01_Clean_data/incendios_clean.csv')

# datos_comunidades <- read.csv(file = './data/nombre_comunidades.csv')

# datos_comunidades <- read.csv(file = './data/nombre_provincias.csv')


# Merge de dos ficheros para obtener el nombre de la comunidad y de la provincia





# Cambiar las variables de tiempo?






# Guardamos el dataset con las variables seleccionadas
write.csv(datos, file = './02_Data_preparation/02_Construct_data/incendios_clean_final.csv')

