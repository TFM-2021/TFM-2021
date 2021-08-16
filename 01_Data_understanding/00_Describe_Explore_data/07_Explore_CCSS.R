

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









indemnizaciones[indemnizaciones=="-"] <- NA

indemnizaciones <- indemnizaciones %>%
  mutate_all(funs(str_replace(.,",","")))%>%
  mutate_all(funs(str_replace(.,",","")))


indemnizaciones[,-c(1,2)] <- apply(indemnizaciones[,-c(1,2)],2, as.numeric)

indemnizaciones %>%
  pivot_longer(!c(ID, PROVINCIA), names_to = "año", values_to = "coste") %>%
  group_by(PROVINCIA, ID) %>%
  summarise(suma = sum(coste,na.rm = T)) %>% 
  ggplot()+
  geom_col(aes(suma,PROVINCIA)) +
  facet_wrap(~ ID )




indemnizaciones_por_año <- indemnizaciones %>%
  pivot_longer(!c(ID, PROVINCIA), names_to = "año", values_to = "coste") %>%
  group_by(PROVINCIA, ID) %>%
  summarise(suma = sum(coste,na.rm = T))
#------------------------------------------------------------------------------

expedientes[expedientes=="-"] <- NA

expedientes <- expedientes %>%
  mutate_all(funs(str_replace(.,",","")))%>%
  mutate_all(funs(str_replace(.,",","")))


expedientes[,-c(1,2)] <- apply(expedientes[,-c(1,2)],2, as.numeric)

expedientes %>%
  pivot_longer(!c(ID, PROVINCIA), names_to = "año", values_to = "coste") %>%
  group_by(PROVINCIA, ID) %>%
  summarise(suma = sum(coste,na.rm = T)) %>% 
  ggplot()+
  geom_col(aes(suma,PROVINCIA)) +
  facet_wrap(~ ID )

expedientes_por_año <- expedientes %>%
  pivot_longer(!c(ID, PROVINCIA), names_to = "año", values_to = "numero") %>%
  group_by(PROVINCIA, ID) %>%
  summarise(numero = sum(numero,na.rm = T)) 



View(expedientes_por_año)
join_expedientes_indemniuzaciones <- inner_join(expedientes_por_año, indemnizaciones_por_año,by=c("PROVINCIA","ID"))

cor(join_expedientes_indemniuzaciones$numero, join_expedientes_indemniuzaciones$suma)



