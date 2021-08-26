

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


# Cluster 1
View(terremotos_cluster_1)
max(terremotos_cluster_1$mag)

fit_cluster_1 <- TFMevt::fitGEV(terremotos_cluster_1$mag, c(0.1,0.1,0.1))
fit_cluster_1


TFMevt::qq_gev(fit_cluster_1,terremotos_cluster_1$mag)

TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         100)

TFMevt::return_level_GEV(fit_cluster_1, 
                         terremotos_cluster_1$mag,
                         100, 
                         "plot")

plot_location
plot_scale
plot_shape

terremotos_cluster_1 %>% View()


TFMevt::mean_excess_GEV(terremotos_cluster_1$mag)

fit_cluster_1_GP <- TFMevt::fitGPD(terremotos_cluster_1$mag, 2.25)
fit_cluster_1_GP

TFMevt::qq_gpd(fit_cluster_1_GP, terremotos_cluster_1$mag)

TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = 2.25,
                         year = 500)


TFMevt::return_level_GPD(fit_cluster_1_GP,
                         terremotos_cluster_1$mag,
                         threshold = 2.25,
                         year = 500,
                         "plot")



fit_cluster_1_Gumbel <- TFMevt::fitGumbel(terremotos_cluster_1$mag)

qq_gumbel(fit_cluster_1_Gumbel, terremotos_cluster_1$mag)
TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            terremotos_cluster_1$mag,
                            year = 100,
                            "plot")





#----------------------------------------------------------------------------

# Cluster 2

max(terremotos_cluster_2$mag)

fit_cluster_2 <- TFMevt::fitGEV(terremotos_cluster_2$mag, c(0.1,0.1,0.1))
fit_cluster_2


TFMevt::qq_gev(fit_cluster_2,terremotos_cluster_2$mag)

TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         500)

TFMevt::return_level_GEV(fit_cluster_2, 
                         terremotos_cluster_2$mag,
                         500, 
                         "plot")

plot_location
plot_scale
plot_shape


TFMevt::mean_excess_GEV(terremotos_cluster_2$mag)

fit_cluster_2_GP <- TFMevt::fitGPD(terremotos_cluster_2$mag, 4.5)
fit_cluster_2_GP

TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = 4.5,
                         year = 500)


TFMevt::return_level_GPD(fit_cluster_2_GP,
                         terremotos_cluster_2$mag,
                         threshold = 4.5,
                         year = 500,
                         "plot")



#-------------------------------------------------------------------------------

# Cluster 3

max(terremotos_cluster_3$mag)

fit_cluster_3 <- TFMevt::fitGEV(terremotos_cluster_3$mag, c(0.1,0.1,0.1))
fit_cluster_3


TFMevt::qq_gev(fit_cluster_3,terremotos_cluster_3$mag)

TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         500)

TFMevt::return_level_GEV(fit_cluster_3, 
                         terremotos_cluster_3$mag,
                         500, 
                         "plot")

plot_location
plot_scale
plot_shape


TFMevt::mean_excess_GEV(terremotos_cluster_3$mag)

fit_cluster_3_GP <- TFMevt::fitGPD(terremotos_cluster_3$mag, 3)
fit_cluster_3_GP

TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = 4.5,
                         year = 500)


TFMevt::return_level_GPD(fit_cluster_3_GP,
                         terremotos_cluster_3$mag,
                         threshold = 4.5,
                         year = 500,
                         "plot")




#-------------------------------------------------------------------------------

# Cluster 4


max(terremotos_cluster_4$mag)

fit_cluster_4 <- TFMevt::fitGEV(terremotos_cluster_4$mag, c(0.1,0.1,0.1))
fit_cluster_4


TFMevt::qq_gev(fit_cluster_4,terremotos_cluster_4$mag)

TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         500)

TFMevt::return_level_GEV(fit_cluster_4, 
                         terremotos_cluster_4$mag,
                         500, 
                         "plot")

plot_location
plot_scale
plot_shape


TFMevt::mean_excess_GEV(terremotos_cluster_4$mag)

fit_cluster_4_GP <- TFMevt::fitGPD(terremotos_cluster_4$mag, 2)
fit_cluster_4_GP

TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = 2,
                         year = 500)


TFMevt::return_level_GPD(fit_cluster_4_GP,
                         terremotos_cluster_4$mag,
                         threshold = 2,
                         year = 500,
                         "plot")

