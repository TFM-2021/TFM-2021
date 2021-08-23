

library(readr)
library(tidyverse)
library(ggcorrplot)
library(plotly)
library(lubridate)

my_data <- readRDS("data/mapa_ESP.rds")

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                             trim_ws = TRUE)



terremotos_ign %>%
  dplyr::group_by(`Tipo Mag.`, Inten.)  %>%
  dplyr::summarise(media = mean(Mag.)) %>%
  ggplot()+
  geom_col(aes(Inten. , media))+
  facet_wrap(~ `Tipo Mag.`)


library(ggstatsplot)

# plot
ggbetweenstats(
  data = terremotos_ign,
  x = Inten.,
  y = Mag.,
  pairwise.comparisons = F,
  title = "Relación magnitud vs intensidad",
  plot.type = "violin",
  ylab = "Magnitud",
  xlab = "Intensidad"
) + coord_flip()

ggbetweenstats(
  data = terremotos_ign,
  x = Inten.,
  y = log(`Prof. (Km)`),
  pairwise.comparisons = F,title = "Relación magnitud vs intensidad"
) + coord_flip()

unique(terremotos_ign$Inten.)
terremotos_ign %>%
  filter(Inten. == "VIII")

terremotos_ign %>%
  filter(Mag. > 5) %>%
  ggplot()+
  geom_jitter(aes(Fecha, Mag.))+
  geom_smooth(aes(Fecha, Mag.))


terremotos_ign %>%
  filter(Mag. > 5) %>%
  ggplot()+
  geom_density(aes(Mag.))


terremotos_con_intensidad <- terremotos_ign %>%
  filter(!is.na(Inten.))

terremotos_con_intensidad_filter <- terremotos_con_intensidad %>%
  filter(Inten. %in% c("VIII","IX-X"))
  
ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos en España",
       subtitle = "Densidad por frecuencia") +
  
  theme_bw() +
  stat_density2d(
    aes(x = Longitud, y = Latitud, fill = ..level.., alpha = 0.25),
    size = 0.01, bins = 50, data = terremotos_ign,
    geom = "polygon"
  )



terremotos_con_intensidad %>%
  ggplot()+
  geom_boxplot(aes(y=Mag.)) +
  #geom_boxplot(aes(y= log(`Prof. (Km)` ))) +
  facet_wrap(~ Inten.)


terremotos_con_intensidad %>%
  group_by(Inten.) %>%
  summarise(n())


terremotos_con_intensidad %>%
  dplyr::filter(Inten. == "IX-X")


terremotos_con_intensidad %>%
  select(Inten., Mag.) %>%
  drop_na()%>%
  group_by(Inten.) %>%
  summarise(media= mean(Mag.)) %>%
  View()
  ggplot(aes(x=Inten. ,y=media))+
  geom_col()
  
  
terremotos_con_intensidad %>%
  select(Inten., `Prof. (Km)`) %>%
  drop_na()%>%
  group_by(Inten.) %>%
  summarise(media= mean(`Prof. (Km)`)) %>%
  #View()
  ggplot(aes(x=Inten. ,y=media))+
  geom_col()




terremotos_con_intensidad %>%
  select(Mag., `Prof. (Km)`, Inten.) %>%
  drop_na()%>%
  ggplot(aes(x=log(Mag.) ,y=log(`Prof. (Km)`)))+
  geom_jitter(aes(color=Inten.))


colnames(terremotos_con_intensidad)

terremotos_con_intensidad %>%
  dplyr::group_by(`Tipo Mag.`)  %>%
  dplyr::summarise(n())



#----------------------------------------------------------------------------

terremotos_sin_intensidad <- terremotos_ign %>%
  filter(!is.na(Inten.))

terremotos_sin_intensidad %>%
  dplyr::group_by(Inten.)  %>%
  dplyr::summarise(n())

ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Municipios de España (peninsula y Baleares)",
       subtitle = "Color por comunidad aut?noma") +
  
  theme_bw() +
  
  geom_point(data=terremotos_sin_intensidad,aes(x= Longitud,
                                                y=  Latitud, color= Inten.))

dim(terremotos_sin_intensidad)
View(terremotos_ign)




#--------------------------------------------------------------------------------

terremotos_ign$Fecha <- as.Date(terremotos_ign$Fecha , format = "%d/%m/%Y")

terremotos_ign %>%
  mutate(date = ymd(Fecha)) %>% 
  mutate_at(vars(Fecha), funs(year, month, day)) %>%
  group_by(year, `Tipo Mag.`) %>%
  summarise(n()) %>%
  View()


#-------------------------------------------------------------------------------

terremotos_ign %>%
  dplyr::filter(Inten. == "IX-X") %>%
  View()

# DISTRIBUCION MODELO ----------------------------------------------------------

terremotos_con_intensidad_filter <- terremotos_con_intensidad %>%
  dplyr::mutate(Inten. = replace(Inten., Inten. == "I", "<IV"),
                Inten. = replace(Inten., Inten. == "I-II", "<IV"),
                Inten. = replace(Inten., Inten. == "II", "<IV"),
                Inten. = replace(Inten., Inten. == "II-III", "<IV"),
                Inten. = replace(Inten., Inten. == "III", "<IV"),
                Inten. = replace(Inten., Inten. == "III-IV", "IV"),
                Inten. = replace(Inten., Inten. == "IV-V", "V")) %>%
  filter(Inten. == "<IV")
ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Municipios de España (peninsula y Baleares)",
       subtitle = "Color por comunidad autónoma") +
  
  theme_bw() +
  
  geom_point(data=terremotos_con_intensidad_filter,aes(x= Longitud,
                                                y=  Latitud,
                                                color=Inten.))


