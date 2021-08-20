library(tidyverse)

my_data <- readRDS("data/mapa_ESP.rds")


ggplot() +

geom_polygon(data = my_data,
             aes(x = long, y = lat, 
                 group = group,col ="white"), 
             color = "black") +

coord_map("mercator") +

labs(title = "Municipios de España (peninsula y Baleares)",
     subtitle = "Color por comunidad aut?noma") +

theme_bw() #+
#geom_point(aes(x= longitud,y=  latitud, 
#               frame=year,
#               ids=nombre, color=provincia))


