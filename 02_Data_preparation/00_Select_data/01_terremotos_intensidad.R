
library(tidyverse)
library(readr)

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
colnames(terremotos_ign)


  

# Selección de atributos o características (columnas)

terremotos_intensidad <-terremotos_ign %>%
    select(`Prof. (Km)`, Inten., Mag., `Tipo Mag.`)

# Selección de elementos (filas)

terremotos_intensidad <- terremotos_intensidad %>%
  filter(`Tipo Mag.` == 4) %>%
  select(!`Tipo Mag.`)

getwd()
saveRDS(terremotos_intensidad, file = "02_Data_preparation/00_Select_data/terremotos_selected_modelo_intensidad.rds")
