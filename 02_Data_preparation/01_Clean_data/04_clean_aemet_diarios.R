
library(readr)
aemet_diarios_join_completo <- read_csv("data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv")


View(aemet_diarios_join_completo)
aemet_diarios_join_completo$fecha <- as.Date(aemet_diarios_join_completo$fecha, format = "%Y-%m-%d")



aemet_diarios_join_completo$prec[aemet_diarios_join_completo$prec == "Ip"] <- 0.1
aemet_diarios_join_completo$prec <- gsub(',', '.', aemet_diarios_join_completo$prec)
aemet_diarios_join_completo$prec <- as.numeric(aemet_diarios_join_completo$prec)


aemet_diarios_join_completo$prec <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$prec))

unique(aemet_diarios_join_completo$dir)
aemet_diarios_join_completo$tmed <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$tmed))
aemet_diarios_join_completo$tmax <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$tmax))/10
aemet_diarios_join_completo$tmin <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$tmin))



aemet_diarios_join_completo$velmedia <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$velmedia))
aemet_diarios_join_completo$dir <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$dir))
aemet_diarios_join_completo$sol <- as.numeric(gsub(',', '.', aemet_diarios_join_completo$sol))



aemet_diarios_join_completo$horatmin <- gsub('Varias', NA, aemet_diarios_join_completo$horatmin)
aemet_diarios_join_completo$horaPresMax <- gsub('Varias', NA, aemet_diarios_join_completo$horaPresMax)
aemet_diarios_join_completo$horaPresMin <- gsub('Varias', NA, aemet_diarios_join_completo$horaPresMin)
aemet_diarios_join_completo$horatmax <- gsub('Varias', NA, aemet_diarios_join_completo$horatmax)


aemet_diarios_join_completo$presMax <- aemet_diarios_join_completo$presMax/10
aemet_diarios_join_completo$presMin <- aemet_diarios_join_completo$presMin/10

aemet_diarios_join_completo$velmedia <- aemet_diarios_join_completo$velmedia * 3.6
aemet_diarios_join_completo$racha <- aemet_diarios_join_completo$racha / 3.6



# provincias_incendios <- read_delim("data/provincias incendios.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

asf

# aemet
# aemet_diarios_join_completo <- 
  
aemet_diarios_join_completo <-  aemet_diarios_join_completo %>%
    filter(!is.na(nombre)) %>%
    group_by(nombre) %>%
    arrange(fecha)%>%
    mutate(diferencia_dia_anterior = presMin - lag(presMax),
           diferencia = presMin - presMax) %>%
    ungroup()
  

aemet_diarios_join_completo %>%
  filter(diferencia < -25)


aemet_diarios_join_completo %>%
  sample_n(size=100000)%>%
  filter(!is.na(diferencia),
         !is.na(prec),
         !is.na(racha))%>%
  ggplot()+
  geom_jitter(aes(diferencia, racha, color=prec))+
  geom_smooth(aes(diferencia, racha))


if((5<0)&&(6<7)){
 "a" 
}

attach(aemet_diarios_join_completo)

aemet_diarios_join_completo <- aemet_diarios_join_completo %>%
  filter(!is.na(racha),
         !is.na(presMin))


aemet_diarios_join_completo$huracan <- NA
for (observacion in 1:nrow(aemet_diarios_join_completo)) {
  
  
  if ((aemet_diarios_join_completo$racha[observacion] > 119)&(aemet_diarios_join_completo$racha[observacion] <153) &(aemet_diarios_join_completo$presMin[observacion] > 979)) {
    
    aemet_diarios_join_completo$huracan[observacion] <- "UNO"
  }else
  if ((aemet_diarios_join_completo$racha[observacion] > 154)&(aemet_diarios_join_completo$racha[observacion] <177) & (aemet_diarios_join_completo$presMin[observacion] < 979)& (aemet_diarios_join_completo$presMin[observacion] > 965)) {
    
    aemet_diarios_join_completo$huracan[observacion] <- "DOS"
  }else
  if ((aemet_diarios_join_completo$racha[observacion] > 178)&(aemet_diarios_join_completo$racha[observacion] <209) &(aemet_diarios_join_completo$presMin[observacion] < 964)& (aemet_diarios_join_completo$presMin[observacion] > 945)) {
    
    aemet_diarios_join_completo$huracan[observacion] <- "TRES"
  }else{
    "a"
  }


  
  
}
aemet_diarios_join_completo <- aemet_diarios_join_completo %>%
  mutate(huracan = replace_na(huracan, "NO HAY"))

aemet_diarios_join_completo %>%
  group_by(huracan) %>%
  summarise(n())


aemet_diarios_join_completo %>%
  group_by(huracan)%>%
  summarise(media = median(diferencia_dia_anterior, na.rm=T))

aemet_diarios_join_completo %>%
  filter(prec> 100) %>% View()
boxplot(aemet_diarios_join_completo$prec)


