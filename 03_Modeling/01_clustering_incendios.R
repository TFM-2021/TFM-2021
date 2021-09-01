library(tidyverse)
library(factoextra)
library(cluster)
install.packages('mapproj')
library(mapproj)


VAL_incendios_EVT <- read.csv("data/data_VAL/incendios_clean_final.csv")

VAL_incendios_EVT <- VAL_incendios_EVT[,-1]


VAL_incendios_EVT <- VAL_incendios_EVT %>% filter(superficie > 50)

VAL_incendios_EVT <- VAL_incendios_EVT %>% filter(lat > 35 & lng > -10 | 
                                                    lat < 30 & lng < -10 )

# lat < 45 & lat > 25 & 
#   lng > -19 & lng < 6

# VAL_incendios_EVT <- VAL_incendios_EVT %>% filter(lat < 30 & lng < -10)



VAL_incendios_EVT_scale <- scale(VAL_incendios_EVT[,c(3,4)] )




# Compute clValid

rows <- sample(nrow(VAL_incendios_EVT_scale))
sample_incendios <- VAL_incendios_EVT_scale[rows, ]



# Elbow method
fviz_nbclust(sample_incendios[1000:3000,], clara, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")


clara.res <- clara(VAL_incendios_EVT_scale , 4, samples = 15, pamLike = T)


clara.res

VAL_incendios_EVT_clusters_clara <- cbind(VAL_incendios_EVT, cluster = clara.res$cluster)



VAL_incendios_EVT_clusters_clara$cluster <- as.factor(VAL_incendios_EVT_clusters_clara$cluster)




mapa_espana <- readRDS("data/mapa_ESP.rds")
ggplot() +
  
  geom_polygon(data = mapa_espana,
               aes(x = long, y = lat, 
                   group = group,col = "white"), 
               color = "black") +
  
  coord_map("mercator") +
  
  labs(title = "Terremotos españa",
       subtitle = "Color por cluster") +
  
  theme_bw() +
  
  geom_point(data = VAL_incendios_EVT_clusters_clara,
             aes(x = lng,
                 y =  lat,
                 color = cluster))




write.csv(VAL_incendios_EVT_clusters_clara, "data/data_VAL/VAL_incendios_EVT_clusters_clara.csv" )

