library(tidyverse)
library(lubridate)


aemet_diarios_join <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv",
                                dec = ",", sep = ",", fileEncoding = "UTF-8")

aemet_diarios_join$fecha <- as.Date(aemet_diarios_join$fecha, format = "%Y-%m-%d")

aemet_diarios_join %>%
  group_by(fecha) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  transmute(fecha, sumNA = rowSums(.[-1])) %>%
  View() # hay fechas vacías


aemet_diarios_join %>% 
  mutate(date = ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day)) %>%
  group_by(year) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  transmute(year, sumNA = rowSums(.[-1])) %>%
  ggplot()+
  geom_col(aes(year, sumNA))
  


aemet_diarios_join %>% 
  mutate(date = ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day)) %>%
  group_by(year)%>%
  summarise(sumYear = n()) %>%
  View()
            
            
  
colnames(aemet_diarios_join)

aemet_diarios_join %>%
  group_by(nombre) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  transmute(nombre, sumNA = rowSums(.[-1])) %>%
  View() # hay fechas vacías



aemet_diarios_join %>%
  mutate(date = ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day)) %>%
  filter(year == 1971) %>%
  select(nombre) %>%
  distinct() %>%
  View()





aemet_diarios_join %>%
  group_by( provincia, nombre) %>%
  summarise_all(~sum(is.na(.))) %>% 
  transmute(provincia, sumNA = rowSums(.[-1])) %>%
  View()
  

sapply(aemet_diarios_join, function(x) sum(is.na(x)))






#------------------------------------------------------------------------------


aemet_diarios_join_completo <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv",
                                dec = ",", sep = ",", fileEncoding = "UTF-8")


aemet_diarios_join_completo$fecha <- as.Date(aemet_diarios_join_completo$fecha, format = "%Y-%m-%d")

aemet_diarios_join_completo %>% 
  mutate(date = ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day)) %>%
  group_by(year) %>% 
  summarise_all(~sum(is.na(.))) %>% 
  transmute(year, sumNA = rowSums(.[-1])) %>%
  ggplot()+
  geom_col(aes(year, sumNA))







