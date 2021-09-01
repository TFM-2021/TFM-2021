
library(tidyverse)
library(readr)

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
colnames(terremotos_ign)


  

# Selección de atributos o características (columnas) y Selección de elementos (filas)

terremotos_intensidad <- terremotos_ign %>%
  dplyr::filter(`Tipo Mag.` %in% c(2, 3, 4)) %>%
    dplyr::select(`Prof. (Km)`, Inten., Mag.,Latitud,Longitud)



terremotos_intensidad <- terremotos_intensidad %>%
  dplyr::filter(!is.na(Inten.))


saveRDS(terremotos_intensidad, file = "02_Data_preparation/00_Select_data/terremotos_selected_modelo_intensidad.rds")
