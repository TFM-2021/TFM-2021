
library(readr)
library(tidyverse)

aemet_clean <- read_csv("data/data_clean/AEMET/aemet_clean.csv")
View(aemet_clean)

aemet_clean %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum)) +
  facet_wrap(~provincia)

aemet_clean %>%
  filter(provincia=="MURCIA") %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum))


attach(aemet_clean)  
Buscar_nulos <- function(data_set, variable){
  
  #for (variable in colnames(data_set)) {
    
    data_set %>%
      select(fecha, {{variable}}) %>%
      filter(is.na(variable)) %>%
      group_by(fecha) %>%
      summarise(missings = n()) %>%
      ggplot() +
      geom_col(aes(fecha, missings/nrow(data_set)*100),
               color="darkgreen")+
      labs(title = paste0("Distribución de NAs por ", variable),
           subtitle = paste0("Porcentaje nulos: ", sum(is.na(variable))/nrow(data_set)*100," %"),
           x = NULL,
           y = "Porcentaje sobre la columna")+
      theme_light()
  #}

}
Buscar_nulos(aemet_clean, indsinop )
