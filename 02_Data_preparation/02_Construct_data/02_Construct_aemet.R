
library(dplyr)
install.packages('stringi')
library(stringi)

# Datos

datos_aemet <- read.csv(file = './02_Data_preparation/01_Clean_data/aemet_clean.csv')





# Latitud y longitud


name <- datos_aemet[1:5, 8]


substr(name,1,nchar(name) - 4)

conversor_coordenadas_decimales <- function(coordenada){
  
  
  horas <- as.numeric(str_sub( coordenada,1,nchar( coordenada)-5))
  
  minutos <- as.numeric(str_sub( coordenada,3,nchar( coordenada)-3))/60
  
  segundos <- as.numeric(str_sub( coordenada,5,nchar( coordenada)-1))/3600
  
  if (str_ends(coordenada, "S")) {
    
    sum(horas,minutos,segundos)*-1
    
  }else if(str_ends(coordenada, "W")){
    
    sum(horas,minutos,segundos)*-1
    
  }else{
    sum(horas,minutos,segundos)
  }
  
  
}


Pruebalat <- datos_aemet[1:5,1]

conversor_coordenadas_decimales(Pruebalat)




# Guardamos el dataset con las variables seleccionadas
write.csv(datos, file = './02_Data_preparation/02_Construct_data/aemet_clean_final.csv')
