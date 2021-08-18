
library(tidyverse)
library(lubridate)


aemet_diarios_join_completo <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv",
                                         dec = ",", sep = ",", fileEncoding = "UTF-8")


aemet_diarios_join_completo$fecha <- as.Date(aemet_diarios_join_completo$fecha, format = "%Y-%m-%d")
View(aemet_diarios_join_completo)


aemet_diarios_join_completo <- aemet_diarios_join_completo %>%
  unique()

recuento_fecha_nombre <- aemet_diarios_join_completo %>% 
  select(fecha, indicativo               ) %>%
  group_by(fecha, indicativo               ) %>%
  summarise(suma= n()) 

recuento_fecha_nombre <- recuento_fecha_nombre %>%
  filter(indicativo != "") %>%
  pivot_wider(names_from = fecha , values_from = suma)

as <- drop_na(recuento_fecha_nombre)
View(as)




aemet_diarios_select <- aemet_diarios_join_completo %>%
  filter(indicativo %in% as$indicativo)
colnames(aemet_diarios_select)

aemet_diarios_select_fecha <- aemet_diarios_select %>%
  group_by(fecha) %>%
  summarise(media_minima = median(tmin,na.rm = T),
            media_media = median(tmed,na.rm = T),
            media_maxima = median(tmax,na.rm = T),
            rango = media_maxima-media_minima) %>%
  mutate(date = lubridate::ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day))# %>%
  

aemet_diarios_select_fecha %>%
  group_by(month)%>%
  summarise(sd = mean(rango))%>%
  ggplot(aes(month, sd))+
  geom_line()+
  geom_smooth(aes(month, sd))





 