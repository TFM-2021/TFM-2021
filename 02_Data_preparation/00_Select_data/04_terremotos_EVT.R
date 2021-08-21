

library(tidyverse)
library(readr)

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
colnames(terremotos_ign)
View(terremotos_EVT)

terremotos_EVT <- terremotos_ign %>%
  select(!c(Evento,
            Hora, 
            `Tipo Mag.` 
            ,Localización,
            Inten.,
            `Prof. (Km)`)) %>%
  filter(!(Latitud < 42 & Longitud < -7 & Latitud > 36.7)) %>%
  filter(!(Latitud < 35 & Longitud > -11.7)) %>%
  drop_na()

my_data <- readRDS("data/mapa_ESP.rds")




ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos españa",
       subtitle = "Color por comunidad aut?noma") +
  
  theme_bw() +
  
  geom_point(data=terremotos_EVT,aes(x= Longitud,
                                                y=  Latitud,
                             color=Mag.))

saveRDS(terremotos_EVT, file = "02_Data_preparation/00_Select_data/terremotos_EVT_selected.rds")

