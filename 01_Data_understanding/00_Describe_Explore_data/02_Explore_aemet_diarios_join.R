
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












