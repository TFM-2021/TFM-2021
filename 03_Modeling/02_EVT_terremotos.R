

library(TFMevt)
library(tidyverse)

VAL_terremotos_EVT_clusters_clara <- readRDS("data/data_VAL/VAL_terremotos_EVT_clusters_clara.rds")

VAL_terremotos_EVT_clusters_clara$fecha <- as.Date(VAL_terremotos_EVT_clusters_clara$fecha, format="%d/%m/%Y")

View(VAL_terremotos_EVT_clusters_clara)



VAL_terremotos_EVT_clusters_clara <- VAL_terremotos_EVT_clusters_clara %>%
  mutate(cluster  = ifelse(cluster == 3 &
                           latitud < 39, 2,cluster) )

VAL_terremotos_EVT_clusters_clara$cluster <- as.factor(VAL_terremotos_EVT_clusters_clara$cluster)

terremotos_cluster_1 <- VAL_terremotos_EVT_clusters_clara %>%
  filter(cluster == 1)%>%
  group_by(fecha)%>%
  summarise(mag = max(mag)) 

terremotos_cluster_2 <- VAL_terremotos_EVT_clusters_clara %>%
  filter(cluster == 2)%>%
  group_by(fecha)%>%
  summarise(mag = max(mag)) 

terremotos_cluster_3 <- VAL_terremotos_EVT_clusters_clara %>%
  filter(cluster == 3)%>%
  group_by(fecha)%>%
  summarise(mag = max(mag)) 

terremotos_cluster_4 <- VAL_terremotos_EVT_clusters_clara %>%
  filter(cluster == 4)%>%
  group_by(fecha)%>%
  summarise(mag = max(mag)) 


terremotos_cluster_1 %>%
  ggplot()+
  geom_line(aes(fecha, mag), color ="tomato") + 
  theme_minimal()+
  ylab("Magnitud")+
  xlab("Fecha")+
  labs(title = "Terremotos en España",
       subtitle = "Zona 1 - Cordillera Pirenaica/ Baleares")


terremotos_cluster_2 %>%
  ggplot()+
  geom_line(aes(fecha, mag), color ="darkgreen") + 
  theme_minimal()+
  ylab("Magnitud")+
  xlab("Fecha")+
  labs(title = "Terremotos en España",
       subtitle = "Zona 2 - Sistema Bético")


terremotos_cluster_3 %>%
  ggplot()+
  geom_line(aes(fecha, mag), color ="cyan3") + 
  theme_minimal()+
  ylab("Magnitud")+
  xlab("Fecha")+
  labs(title = "Terremotos en España",
       subtitle = "Zona 3 - Macizo Galaico")


terremotos_cluster_4 %>%
  ggplot()+
  geom_line(aes(fecha, mag), color ="purple") + 
  theme_minimal()+
  ylab("Magnitud")+
  xlab("Fecha")+
  labs(title = "Terremotos en España",
       subtitle = "Zona 4 - Islas Canarias")





# EVT---------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# cluster_1
quantile(terremotos_cluster_1$mag)
1/
fit_cluster_1 <- TFMevt::fitGEV(terremotos_cluster_1$mag, c(0.1,0.1,0.1))
fit_cluster_1


TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         5)
TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         10)
TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         25)
TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         50)
TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         100)

TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         100, 
                         "plot")




TFMevt::mean_excess_GEV(terremotos_cluster_1$mag)



threshold <- 3

fit_cluster_1_GP <- TFMevt::fitGPD(terremotos_cluster_1$mag, threshold)
fit_cluster_1_GP



TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_1_Gumbel <- TFMevt::fitGumbel(terremotos_cluster_1$mag)
fit_cluster_1_Gumbel
extRemes::fevd(terremotos_cluster_1$mag, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 100,
                            "plot")


#-------------------------------------------------------------------------------
# cluster_2
max(terremotos_cluster_2$mag)
fit_cluster_2 <- TFMevt::fitGEV(terremotos_cluster_2$mag, c(0.1,0.1,0.1))
fit_cluster_2


TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         5)
TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         10)
TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         25)
TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         50)
TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         100)

TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         100, 
                         "plot")




TFMevt::mean_excess_GEV(terremotos_cluster_2$mag)



threshold <- 5

fit_cluster_2_GP <- TFMevt::fitGPD(terremotos_cluster_2$mag, threshold)
fit_cluster_2_GP



TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_2_Gumbel <- TFMevt::fitGumbel(terremotos_cluster_2$mag)
fit_cluster_2_Gumbel



TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            terremotos_cluster_2$mag,
                            year = 100,
                            "plot")



#-------------------------------------------------------------------------------
# cluster_3
summary(terremotos_cluster_3$mag)
fit_cluster_3 <- TFMevt::fitGEV(terremotos_cluster_3$mag, c(0.1,0.1,0.1))
fit_cluster_3


TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         5)
TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         10)
TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         25)
TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         50)
TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         100)

TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         100*365, 
                         "plot")



fit <- extRemes::fevd(terremotos_cluster_3$mag)
extRemes::return.level(fit, return.period = 365*5)
TFMevt::mean_excess_GEV(terremotos_cluster_3$mag)



threshold <- 3.2

fit_cluster_3_GP <- TFMevt::fitGPD(terremotos_cluster_3$mag, threshold)
fit_cluster_3_GP



TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_3_Gumbel <- TFMevt::fitGumbel(terremotos_cluster_3$mag)
fit_cluster_3_Gumbel
extRemes::fevd(terremotos_cluster_3$mag, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            terremotos_cluster_3$mag,
                            year = 100,
                            "plot")





#-------------------------------------------------------------------------------
# cluster_4
max(terremotos_cluster_4$mag)
fit_cluster_4 <- TFMevt::fitGEV(terremotos_cluster_4$mag, c(0.1,0.1,0.1))
fit_cluster_4


TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         5)
TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         10)
TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         25)
TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         50)
TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         100)

TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         100*365, 
                         "plot")




TFMevt::mean_excess_GEV(terremotos_cluster_4$mag)



threshold <- 3

fit_cluster_4_GP <- TFMevt::fitGPD(terremotos_cluster_4$mag, threshold)
fit_cluster_4_GP



TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_4_Gumbel <- TFMevt::fitGumbel(terremotos_cluster_4$mag)
fit_cluster_4_Gumbel
extRemes::fevd(terremotos_cluster_4$mag, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            terremotos_cluster_4$mag,
                            year = 100,
                            "plot")


