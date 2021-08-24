

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
  filter(Fecha == "2011-05-11") %>%
  View()

terremotos_ign %>%
  filter(!is.na(Inten.)) %>%
  dplyr::group_by(`Tipo Mag.`)  %>%
  dplyr::summarise(mean(Mag.)) %>%View()

terremotos_ign_mb <- terremotos_ign %>%
  filter(!is.na(Inten.),
         `Tipo Mag.` %in% c(2,3,4,13)) 
   
  

ggplot(terremotos_ign_mb)+
  geom_point(aes(log(`Prof. (Km)`), Mag., color=Inten.))+
  theme_minimal()+
  ylab("Magnitud")+
  xlab("Profundidad en logaritmos")+
  labs(title = "Terremotos en España",
       subtitle = "Relación profundidad, intensidad y magnitud")









terremotos_ign_mb %>%
  filter(Inten. == "II",
         Mag. > 5.5) %>% View()

quantile(terremotos_ign_mb$`Prof. (Km)`,na.rm = T)

terremotos_ign %>%
  dplyr::group_by(`Tipo Mag.`, Inten.)  %>%
  dplyr::summarise(media = mean(Mag.)) %>%
  ggplot()+
  geom_col(aes(Inten. , media))+
  facet_wrap(~ `Tipo Mag.`)


library(ggstatsplot)

# plot
ggbetweenstats(
  data = terremotos_ign_mb,
  x = Inten.,
  y = Mag.,
  pairwise.comparisons = F,
  title = "Relación magnitud vs intensidad",
  plot.type = "violin",
  ylab = "Magnitud",
  xlab = "Intensidad",
  palette = "Set3"
) + coord_flip()

terremotos_ign_mb$`Prof. (Km)` <- log(terremotos_ign_mb$`Prof. (Km)`)
ggbetweenstats(
  data = terremotos_ign_mb,
  x = Inten.,
  y = `Prof. (Km)`,
  pairwise.comparisons = F,
  title = "Relación profundidad vs intensidad",
  plot.type = "violin",
  ylab = "Profundidad (en logaritmos)",
  xlab = "Intensidad",
  palette = "Set3"
) + coord_flip()



terremotos_ign_mb_oiutlier <- terremotos_ign_mb %>%
  filter(Inten. %in% c("I-II","II","II-III")) %>%
  mutate(Outlier = ifelse(Mag.>4.5, "SI","NO"))













ggplot(terremotos_ign_mb)+
  geom_jitter(aes( x = log(Mag.),
                   y = `Prof. (Km)`))+
  geom_smooth(aes( x = log(Mag.),
                   y = `Prof. (Km)`))

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
  filter(Inten. %in% c("VIII","IX-X", "VII","VI", "V"))
  
ggplot() +
  
  geom_polygon(data = my_data,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos en España",
       subtitle = "Outliers intensidades I a III") +
  
  theme_bw() +
  geom_point(data = terremotos_con_intensidad%>%
               filter(`Prof. (Km)`< 2), 
             aes(Longitud, Latitud, color = `Prof. (Km)`))


quantile(terremotos_con_intensidad$`Prof. (Km)`, na.rm = T)

terremotos_ign_mb_oiutlier %>%
  group_by(Outlier) %>%
  summarise(median(`Prof. (Km)`, na.rm=T))

A <-terremotos_ign %>%
  filter(Latitud <37,
         Longitud> -5.3,
         `Prof. (Km)`<2)

median(A$Mag., na.rm = T)



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
  
  geom_point(data=terremotos_ign_mb,aes(x= Longitud,
                                                y=  Latitud, color= Inten., size= prof))

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
  
  theme_minimal() +
  
  geom_point(data=terremotos_con_intensidad_filter,aes(x= Longitud,
                                                y=  Latitud,
                                                color=Inten.))




# FRECUENCIA---------------------------------------------------------------------



my_data <- readRDS("data/mapa_ESP.rds")

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                             trim_ws = TRUE)
library(lubridate)
terremotos_ign %>%
  filter(!is.na(Inten.),
         Inten. != "-1",
         Inten. != "-1.0",
         `Tipo Mag.` %in% c(2,3,4,13)) %>%
  
  dplyr::mutate(date = ymd(Fecha)) %>% 
  mutate_at(vars(Fecha), funs(year, month, day)) %>%
  group_by(day,Inten.)%>% 
  summarise(count = n()) %>%
  ggplot()+
  geom_bar(aes(year, fill = Inten.),position = "fill")





terremotos_ign %>%
  filter(!is.na(Inten.),
         Inten. != "-1",
         Inten. != "-1.0",
         `Tipo Mag.` %in% c(2,3,4,13)) %>%
  mutate(lugar = ifelse(Latitud > 40, "NORTE",
                        if_else(Latitud > 34, "SUR", "CANARIAS"))) %>%
  dplyr::mutate(date = ymd(Fecha)) %>% 
  mutate_at(vars(Fecha), funs(year, month, day)) %>%
  group_by(year, month, lugar) %>%
  summarise(max = n()) %>%
  ggplot()+
  geom_col(aes(month, max, fill=lugar))+
  facet_wrap( ~lugar)+
  theme_minimal()+
  ylab("Números por mes")+
  xlab("Fecha")+
  labs(title = "Terremotos en España",
       subtitle = "Frecuencia según zona de España")+
  theme(legend.position = "none")
colnames(terremotos_ign)

