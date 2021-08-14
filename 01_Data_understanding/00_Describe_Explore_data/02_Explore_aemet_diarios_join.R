library(e1071)  
library(tidyverse)

aemet_diarios_join <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv",
                                dec = ",", sep = ",",fileEncoding = "utf-8")

aemet_diarios_join$fecha <- as.Date(aemet_diarios_join$fecha, format = "%Y-%m-%d")



aemet_diarios_join$prec[aemet_diarios_join$prec == "Ip"] <- 0.1
aemet_diarios_join$prec <- gsub(',', '.', aemet_diarios_join$prec)
aemet_diarios_join$prec <- as.numeric(aemet_diarios_join$prec)




aemet_diarios_join_nums <- aemet_diarios_join[, unlist(lapply(aemet_diarios_join, is.numeric))]
aemet_diarios_join_nums <- drop_na(aemet_diarios_join_nums)





colnames(aemet_diarios_join)




set.seed(123)

sample_aemet <- aemet_diarios_join[sample(1:nrow(aemet_diarios_join), 200000), ]

sample_aemet$fecha <- as.Date(sample_aemet$fecha, format = "%Y-%m-%d")

sample_aemet %>%
  select(fecha, tmed) %>%
  filter(!is.na(tmed)) %>%
  group_by(fecha) %>%
  summarise(media = mean(tmed)) %>% 
  ggplot()+
  geom_line(aes(fecha, media)) +
  geom_smooth(aes(fecha, media))


aemet_diarios_join %>%
  select(provincia, tmed)%>%
  filter(!is.na(tmed)) %>%
  group_by(provincia) %>%
  summarise(media = mean(tmed))%>%
  View()


aemet_diarios_join %>%
  select(provincia, prec)%>%
  filter(!is.na(prec)) %>%
  group_by(provincia) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            cv = sd(prec)/mean(prec)*100)%>%
  View()


aemet_diarios_join %>%
  mutate(month = format(fecha, "%m")) %>%
  select(provincia, prec, month)%>%
  filter(!is.na(prec)) %>%
  group_by(provincia, month) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            cv = sd(prec)/mean(prec)*100)%>%
  View()



aemet_diarios_join %>%
  mutate(year = format(fecha, "%Y")) %>%
  select(provincia, prec, year)%>%
  filter(!is.na(prec)) %>%
  group_by(provincia, year) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            cv = sd(prec)/mean(prec)*100)%>%
  View()



aemet_diarios_join %>%
  mutate(year = as.Date(format(fecha, "%Y"),format = "%Y")) %>%
  select( prec, year)%>%
  filter(!is.na(prec)) %>%
  group_by( year) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            cv = sd(prec)/mean(prec)*100)%>%
  ggplot()+
  geom_col(aes(year, cv))


ggcorrplot::ggcorrplot(cor(aemet_diarios_join_nums),type = "lower")



# Estadísticos descriptivos

  # fecha

aemet_diarios_join %>%
  mutate(year = as.Date(format(fecha, "%Y"),format = "%Y")) %>%
  select( prec, year)%>%
  filter(!is.na(prec)) %>%
  group_by( year) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            cv = sd(prec)/mean(prec)*100,
            skew = skewness(prec),
            kurtosis = kurtosis(prec)) %>%
  View()



aemet_diarios_join %>%
  mutate(year = as.Date(format(fecha, "%Y"),format = "%Y")) %>%
  select( velmedia, year)%>%
  filter(!is.na(velmedia)) %>%
  group_by( year) %>%
  summarise(media = mean(velmedia),
            sd = sd(velmedia),
            max = max(velmedia),
            min  = min(velmedia),
            cv = sd(velmedia)/mean(velmedia)*100,
            skew = skewness(velmedia),
            kurtosis = kurtosis(velmedia)) %>%
  View()

aemet_diarios_join %>%
  mutate(year = as.Date(format(fecha, "%Y"),format = "%Y")) %>%
  select( racha, year)%>%
  filter(!is.na(racha)) %>%
  group_by( year) %>%
  summarise(media = mean(racha),
            sd = sd(racha),
            max = max(racha),
            min  = min(racha),
            cv = sd(racha)/mean(racha)*100,
            skew = skewness(racha),
            kurtosis = kurtosis(racha)) %>%
  View()


  # provincia


aemet_diarios_join %>%
  select( prec, provincia)%>%
  filter(!is.na(prec)) %>%
  group_by( provincia) %>%
  summarise(media = mean(prec),
            sd = sd(prec),
            max = max(prec),
            min  = min(prec),
            rango = max(prec)-min(prec),
            cv = sd(prec)/mean(prec)*100,
            skew = skewness(prec),
            kurtosis = kurtosis(prec)) %>%
  View()



aemet_diarios_join %>%
  select( velmedia, provincia)%>%
  filter(!is.na(velmedia)) %>%
  group_by( provincia) %>%
  summarise(media = mean(velmedia),
            sd = sd(velmedia),
            max = max(velmedia),
            min  = min(velmedia),
            rango = max(velmedia)-min(velmedia),
            cv = sd(velmedia)/mean(velmedia)*100,
            skew = skewness(velmedia),
            kurtosis = kurtosis(velmedia)) %>%
  View()

aemet_diarios_join %>%
  select( racha, provincia)%>%
  filter(!is.na(racha)) %>%
  group_by( provincia) %>%
  summarise(media = mean(racha),
            sd = sd(racha),
            max = max(racha),
            min  = min(racha),
            rango = max(racha)-min(racha),
            cv = sd(racha)/mean(racha)*100,
            skew = skewness(racha),
            kurtosis = kurtosis(racha)) %>%
  View()


# las provinias esperadas como Murica o valencia tiene el mayo rango de precipitacion media



#-------------------------------------------------------------------------------

aemet_diarios_join$racha



racha_cyl <- aemet_diarios_join %>%
  select(provincia, racha, fecha) %>%
  group_by(provincia, fecha) %>%
  summarise(suma = mean(racha)) %>%
  filter(provincia %in% c("LEON","SORIA",  "BURGOS",               
                          "SEGOVIA", "PALENCIA",             
                           "VALLADOLID" , "AVILA", "SALAMANCA", "ZAMORA")) %>%
  group_by(fecha)%>%
  summarise(mean_cyl = mean(suma)) 

ggplot(racha_cyl)+
  geom_line(aes(fecha, mean_cyl))





boxplot(aemet_diarios_join$prec)

aemet_diarios_join %>% 
  filter(provincia == "MADRID",
         fecha > "1989-12-01",
         fecha < "1990-01-01") %>%
  View()

  
# DATOS DE LA NUEVA EXTRACCIÓN CON DATOS DE AÑOS COMPLETOS-----------------------





aemet_diarios_join_completo <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv",
                                         dec = ",", sep = ",", fileEncoding = "UTF-8")


aemet_diarios_join_completo$fecha <- as.Date(aemet_diarios_join_completo$fecha, format = "%Y-%m-%d")

# distribuvion de las estaciones por años 

aemet_diarios_join_completo_fechas <- aemet_diarios_join_completo %>% 
  dplyr::mutate(date = ymd(fecha)) %>% 
  mutate_at(vars(fecha), funs(year, month, day))



library(gapminder)
data("gapminder")
View(gapminder)



library(plotly)

# cargo aemet diario para obtener las coordenadas de las estaciones

aemet_mensual <- read.csv2("data/data_raw/AEMET/data_join_aemet.csv",
                                         dec = ",", sep = ",", fileEncoding = "UTF-8")

colnames(aemet_mensual)

TMP_coordendas_estaciones <- tibble("nombre" =aemet_mensual$nombre,
                                    "latitud" = aemet_mensual$latitud,
                                    "longitud" = aemet_mensual$longitud) %>%
                                      distinct()


conversor_coordenadas_decimales <- function(coordenada){
  
 
  horas <- as.numeric(str_sub( coordenada,1,nchar( coordenada)-5))
  
  minutos <- as.numeric(str_sub( coordenada,3,nchar( coordenada)-3))/60
  
  segundos <- as.numeric(str_sub( coordenada,5,nchar( coordenada)-1))/3600
  
  if (str_ends(coordenada, "S")) {
    
    sum(horas,minutos,segundos)*-1
    
  }else if(str_ends(coordenada, "W")){
    
    sum(horas,minutos,segundos)*-1
    
  }else{
    sum(horas,minutos,segundos)
  }
  
  
}


aemet_plot <- aemet_diarios_join_completo_fechas %>%
  select(year, provincia, nombre) %>%
  distinct()

aemet_plot <- inner_join(TMP_coordendas_estaciones, aemet_plot, by="nombre")


aemet_plot[,2] <- apply(aemet_plot[,2],1, conversor_coordenadas_decimales)
aemet_plot[,3] <- apply(aemet_plot[,3],1, conversor_coordenadas_decimales)






aemet_plot %>%
  filter(nombre=="PUERTO DE LEITARIEGOS")
as.numeric(aemet_plot[aemet_plot$nombre == "PUERTO DE LEITARIEGOS",][1,2])
as.numeric(TMP_coordendas_estaciones[TMP_coordendas_estaciones$nombre == "PUERTO DE LEITARIEGOS",][1,2])





