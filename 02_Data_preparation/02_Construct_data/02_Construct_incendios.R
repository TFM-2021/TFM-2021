
library(dplyr)
library(tidygeocoder)
library(maps)
library(ggrepel)

# Datos

datos_incendios <- read.csv(file = './02_Data_preparation/01_Clean_data/incendios_clean.csv')

datos_comunidades <- read.csv(file = './data/comunidades incendios.csv', 
                              sep  = ';' , encoding = 'UTF-8')

datos_provincias <- read.csv(file = './data/provincias incendios.csv',  
                             sep  = ';' , encoding = 'UTF-8')



datos_incendios <- datos_incendios[,-c(1:2)]



datos_comunidades <- rename(datos_comunidades, 'idcomunidad' = 'X.U.FEFF.idcomunidad')
datos_provincias <- rename(datos_provincias, 'idprovincia' = 'X.U.FEFF.idprovincia' )


# Merge de dos ficheros para obtener el nombre de la comunidad y de la provincia


datos1 <- merge(datos_incendios, datos_comunidades, by = "idcomunidad", all.x = TRUE )


datos2 <- merge(datos1, datos_provincias, by = "idprovincia", all.x = TRUE)

datos_finales <- datos2[,-c(1:2)]


ggplot(datos_finales, aes(lng, lat), color = "grey99") +
  borders("state") + geom_point() +
  geom_label_repel(aes(label = provincia)) +
  theme_void()


# Cambiar las variables de tiempo?






# Guardamos el dataset con las variables seleccionadas
write.csv(datos, file = './02_Data_preparation/02_Construct_data/incendios_clean_final.csv')

