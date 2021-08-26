
library(tidyverse)
library(factoextra)
library(cluster)

VAL_terremotos_EVT <- readRDS("data/data_VAL/VAL_terremotos_EVT.rds")

VAL_terremotos_EVT_scale <- scale(VAL_terremotos_EVT[,c(-1,-4)] )
 



# Compute clValid

rows <- sample(nrow(VAL_terremotos_EVT_scale))
sample_terremotos <- VAL_terremotos_EVT_scale[rows, ]



# Elbow method
fviz_nbclust(sample_terremotos[20000:35000,], clara, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")




clara.res <- clara(VAL_terremotos_EVT_scale , 4, samples = 15, pamLike = T)


clara.res

VAL_terremotos_EVT_clusters_clara <- cbind(VAL_terremotos_EVT, cluster = clara.res$cluster)



VAL_terremotos_EVT_clusters_clara$cluster <- as.factor(VAL_terremotos_EVT_clusters_clara$cluster)




mapa_españa <- readRDS("data/mapa_ESP.rds")
ggplot() +
  
  geom_polygon(data = mapa_españa,
               aes(x = long, y = lat, 
                   group = group,col ="white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos españa",
       subtitle = "Color por cluster") +
  
  theme_bw() +
  
  geom_point(data=VAL_terremotos_EVT_clusters_clara,
             aes(x= longitud,
                 y=  latitud,
                 color=cluster))
 



saveRDS(VAL_terremotos_EVT_clusters_clara, "data/data_VAL/VAL_terremotos_EVT_clusters_clara.rds")







