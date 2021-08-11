

library(readr)
library(tidyverse)
library(ggcorrplot)


expedientes <- read_delim("data/data_raw/CCSS/expedientes.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

indemnizaciones <- read_delim("data/data_raw/CCSS/indemnizaciones.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos <- read_delim("data/data_raw/CCSS/grandes_eventos.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos_desglosados <- read_delim("data/data_raw/CCSS/grandes_eventos_desglosados.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)


colnames(expedientes)

#------------------------------------------------------------------------------
View(indemnizaciones)


indemnizaciones <- data.frame(lapply(indemnizaciones, function(x) {
  gsub("-", "", x)}))
indemnizaciones <- data.frame(lapply(indemnizaciones, function(x) {
  gsub(",", ".", x)}))


indemnizaciones[,-c(1,2)] <- data.frame(lapply(indemnizaciones[,-c(1,2)], as.numeric))


indemnizaciones %>% 
  dplyr::filter(ID == "inundaciones") %>%
  select(!ID) %>%
  pivot_longer( !PROVINCIA, names_to = "año", values_to = "count" ) %>%
  View()

indemnizaciones %>% 
  dplyr::filter(ID == "teremoto") %>%
  select(!ID) %>%
  pivot_longer( !PROVINCIA, names_to = "año", values_to = "count" ) %>%
  View()





indemnizaciones %>% 
  select(!ID) %>%
  pivot_longer( !PROVINCIA, names_to = "año", values_to = "count" ) %>%
  filter(!is.na(count))%>%
  group_by(PROVINCIA) %>%
  summarise(suma = sum(count)) %>%
  View()
#----------------------------------------------------------------------------



View(grandes_eventos)

grandes_eventos$media <- grandes_eventos$Indemnizaciones/grandes_eventos$`Nº de Reclamaciones`
