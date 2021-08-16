library(readr)
library(tidyverse)

expedientes <- read_delim("data/data_raw/CCSS/expedientes.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

indemnizaciones <- read_delim("data/data_raw/CCSS/indemnizaciones.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos <- read_delim("data/data_raw/CCSS/grandes_eventos.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos_desglosados <- read_delim("data/data_raw/CCSS/grandes_eventos_desglosados.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)

View(grandes_eventos_desglosados)
datos_aemet <- read.csv(file = './data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv')



grandes_eventos_join <- left_join(grandes_eventos_desglosados, grandes_eventos, "ID")
View(grandes_eventos_join)


grandes_eventos_join_select <- grandes_eventos_join %>%
  select(`CLASE DE RIESGO`,
         Indemnizaciones.x,
         `Mes y Año de Ocurrencia`,
         `Lugar de Ocurrencia`,
         `Causa del Siniestro*`)


View(grandes_eventos_join_select)



saveRDS(grandes_eventos_join_select, file = "02_Data_preparation/00_Select_data/inundaciones_TCA_selected.rds")


