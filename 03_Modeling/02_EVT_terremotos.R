

library(TFMevt)

VAL_terremotos_EVT_clusters_clara <- readRDS("data/data_VAL/VAL_terremotos_EVT_clusters_clara.rds")

VAL_terremotos_EVT_clusters_clara$fecha <- as.Date(VAL_terremotos_EVT_clusters_clara$fecha, format="%d/%m/%Y")

View(VAL_terremotos_EVT_clusters_clara)

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


View(terremotos_cluster_1)


terremotos_cluster_1 %>%
  ggplot()+
  geom_line(aes(fecha, mag))


terremotos_cluster_2 %>%
  ggplot()+
  geom_line(aes(fecha, mag))


terremotos_cluster_3 %>%
  ggplot()+
  geom_line(aes(fecha, mag))


terremotos_cluster_4 %>%
  ggplot()+
  geom_line(aes(fecha, mag))





# EVT---------------------------------------------------------------------------


# Cluster 1


TFMevt::fitGEV(terremotos_cluster_1$mag, c(0.1,0.1,0.1))

extRemes::fevd(terremotos_cluster_1$mag, type = "GP",threshold = 4)
extRemes::f
TFMevt::fitGPD(terremotos_cluster_1$mag, 4)
plot_location
plot_scale
plot_shape








