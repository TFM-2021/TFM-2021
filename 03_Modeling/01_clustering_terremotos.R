
library(tidyverse)
library(factoextra)


VAL_terremotos_EVT <- readRDS("data/data_VAL/VAL_terremotos_EVT.rds")

VAL_terremotos_EVT_scale <- scale(VAL_terremotos_EVT[,c(-1,-4)] )
 



saveRDS(summary(intern), "03_Modeling/cluter_terremoto.rds")


library(clValid)
# Iris data set:
# - Remove Species column and scale


# Compute clValid

rows <- sample(nrow(VAL_terremotos_EVT_scale))
sample_terremotos <- VAL_terremotos_EVT_scale[rows, ]

clmethods <- c("clara","kmeans","pam")
intern <- clValid(sample_terremotos[1:10000,], nClust = 2:5,
                  clMethods = clmethods, 
                  validation = c("internal", "stability", "biological"))
summary(intern)


km.res <- kmeans(VAL_terremotos_EVT_scale, 4, nstart = 25)
clara.res <- clara(VAL_terremotos_EVT_scale, 4, samples = 50, pamLike = TRUE)


VAL_terremotos_EVT_clusters_kmeans <- cbind(VAL_terremotos_EVT, cluster = km.res$cluster)
VAL_terremotos_EVT_clusters_clara <- cbind(VAL_terremotos_EVT, cluster = clara.res$cluster)





VAL_terremotos_EVT_clusters_kmeans$cluster <- as.factor(VAL_terremotos_EVT_clusters_kmeans$cluster)
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
  
  geom_point(data=dd,
             aes(x= longitud,
                 y=  latitud,
                 color=cluster))
 



saveRDS(VAL_terremotos_EVT_clusters_clara, "data/data_VAL/VAL_terremotos_EVT_clusters_clara.rds")







