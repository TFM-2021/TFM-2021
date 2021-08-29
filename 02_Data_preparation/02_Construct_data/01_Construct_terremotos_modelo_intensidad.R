library(tidyverse)

terremotos_modelo_intensidad <- readRDS(file = "02_Data_preparation/01_Clean_data/terremotos_clean_modelo_intensidad.rds")
my_data <- readRDS("data/mapa_ESP.rds")
unique(terremotos_modelo_intensidad$inten)

terremotos_modelo_intensidad <- terremotos_modelo_intensidad %>%
  dplyr::mutate(inten = replace(inten, inten == "I", "<IV"),
                inten = replace(inten, inten == "I-II", "<IV"),
                inten = replace(inten, inten == "II", "<IV"),
                inten = replace(inten, inten == "II-III", "<IV"),
                inten = replace(inten, inten == "III", "<IV"),
                inten = replace(inten, inten == "III-IV", "IV"),
                inten = replace(inten, inten == "IV-V", "V"),
                inten = replace(inten, inten == "VIII", ">VII"),
                inten = replace(inten, inten == "IX-X", ">VII"))


terremotos_modelo_intensidad$inten <- as.factor(terremotos_modelo_intensidad$inten)
terremotos_modelo_intensidad_filter <- terremotos_modelo_intensidad %>%
  filter(inten !="<IV")

ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos de España") +
  
  theme_bw() +
  
  geom_point(data=terremotos_modelo_intensidad_filter,aes(x= longitud,
                                                y=  latitud, color= inten))




terremotos_modelo_intensidad <- terremotos_modelo_intensidad %>%
  dplyr::mutate(placa_tectonica = as.factor(ifelse((latitud  <= 39 &
                                               latitud >= 32), "1","0"))) %>%
  select(!c(latitud, longitud))

unique(terremotos_modelo_intensidad$inten)
  

saveRDS(terremotos_modelo_intensidad, "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")

