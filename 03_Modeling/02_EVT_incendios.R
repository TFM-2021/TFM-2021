library(TFMevt)
library(tidyverse)


VAL_incendios_EVT_clusters_clara <- read.csv("data/data_VAL/VAL_incendios_EVT_clusters_clara.csv")

VAL_incendios_EVT_clusters_clara$fecha <- as.Date(VAL_incendios_EVT_clusters_clara$fecha)

VAL_incendios_EVT_clusters_clara <- VAL_incendios_EVT_clusters_clara[,-1]

View(VAL_incendios_EVT_clusters_clara)


# VAL_incendios_EVT_clusters_clara <- VAL_incendios_EVT_clusters_clara %>%
#   mutate(cluster  = ifelse(cluster == 3 &
#                              lat < 39, 2,cluster) )

VAL_incendios_EVT_clusters_clara$cluster <- as.factor(VAL_incendios_EVT_clusters_clara$cluster)

incendios_cluster_1 <- VAL_incendios_EVT_clusters_clara %>%
  filter(cluster == 1) %>%
  group_by(fecha) %>%
  summarise(superficie = max(superficie)) 

mean(incendios_cluster_1$superficie) # mediana del conjunto
max(incendios_cluster_1$superficie) # incendio de mayor superficienitud



incendios_cluster_2 <- VAL_incendios_EVT_clusters_clara %>% 
  filter(cluster == 2) %>%
  group_by(fecha) %>%
  summarise(superficie = max(superficie)) 

mean(incendios_cluster_2$superficie) # mediana del conjunto
max(incendios_cluster_2$superficie) # incendio de mayor superficienitud

incendios_cluster_3 <- VAL_incendios_EVT_clusters_clara %>%
  filter(cluster == 3) %>%
  group_by(fecha) %>%
  summarise(superficie = max(superficie)) 

mean(incendios_cluster_3$superficie) # mediana del conjunto
max(incendios_cluster_3$superficie) # incendio de mayor superficienitud

incendios_cluster_4 <- VAL_incendios_EVT_clusters_clara %>%
  filter(cluster == 4) %>%
  group_by(fecha) %>%
  summarise(superficie = max(superficie))

mean(incendios_cluster_4$superficie) # mediana del conjunto
max(incendios_cluster_4$superficie) # incendio de mayor superficienitud
# isuperficieen 1.3.1.1.1.	Extreme Value Theoy para zona 1 Cordillera Pirenaica / Baleares

incendios_cluster_1 %>%
  ggplot() +
  geom_line(aes(fecha, superficie), color = "tomato") + 
  theme_minimal() +
  ylab("Superficie quemada") +
  xlab("Fecha") +
  labs(title = "incendios en España",
       subtitle = "Zona 1 - Zona norte de España y parte de Castilla y León") 




incendios_cluster_2 %>%
  ggplot() +
  geom_line(aes(fecha, superficie), color ="darkgreen") + 
  theme_minimal() +
  ylab("Superficie quemada") +
  xlab("Fecha") +
  labs(title = "incendios en España",
       subtitle = "Zona 2 - Zona Este del apenínsula y Baleares")


incendios_cluster_3 %>%
  ggplot()+
  geom_line(aes(fecha,superficie), color ="cyan3") + 
  theme_minimal()+
  ylab("superficie quemada")+
  xlab("Fecha")+
  labs(title = "Incendios en España",
       subtitle = "Zona 3 - Zona Centro y sur de la península incluida Canarias")


incendios_cluster_4 %>%
  ggplot()+
  geom_line(aes(fecha, superficie), color ="purple") +
  theme_minimal()+
  ylab("Superficie quemada")+
  xlab("Fecha")+
  labs(title = "incendios en España",
       subtitle = "Zona 4 - Comunidad de Galicia y parte de Castilla y León")





# EVT---------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# cluster_1
quantile(incendios_cluster_1$superficie)

fit_cluster_1 <- TFMevt::fitGEV(incendios_cluster_1$superficie, c(0.1,0.1,0.1))
fit_cluster_1 # AIC, BIC, Valores optimos (parametrso estimados), desviacion tipica estimada, Negative log likelihood


TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         5)
TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         10)
TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         25)
TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         50)
TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         100)

TFMevt::return_level_GEV(fit_cluster_1, 
                         incendios_cluster_1$superficie,
                         100, 
                         "plot") # Shape de GEV




TFMevt::mean_excess_GEV(incendios_cluster_1$superficie) # Gráfica mean excess

# Como se ve en el gráfico superior, la forma geométrica de la función del mean excess cambia a una forma más lineal después de la superficienitud 3


threshold <- 500

fit_cluster_1_GP <- TFMevt::fitGPD(incendios_cluster_1$superficie, threshold)
fit_cluster_1_GP # AIC, BIC, Valores optimos (parametrso estimados), desviacion tipica estimada, Negative log likelihood



TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_1_GP,
                         incendios_cluster_1$superficie,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_1_Gumbel <- TFMevt::fitGumbel(incendios_cluster_1$superficie)
fit_cluster_1_Gumbel
extRemes::fevd(incendios_cluster_1$superficie, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_1_Gumbel,
                            incendios_cluster_1$superficie,
                            year = 100,
                            "plot")


#-------------------------------------------------------------------------------
# cluster_2
max(incendios_cluster_2$superficie)
fit_cluster_2 <- TFMevt::fitGEV(incendios_cluster_2$superficie, c(0.1,0.1,0.1))
fit_cluster_2


TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         5)
TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         10)
TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         25)
TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         50)
TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         100)

TFMevt::return_level_GEV(fit_cluster_2, 
                         incendios_cluster_2$superficie,
                         100, 
                         "plot")




TFMevt::mean_excess_GEV(incendios_cluster_2$superficie)



threshold <- 5000

fit_cluster_2_GP <- TFMevt::fitGPD(incendios_cluster_2$superficie, threshold)
fit_cluster_2_GP



TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_2_GP,
                         incendios_cluster_2$superficie,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_2_Gumbel <- TFMevt::fitGumbel(incendios_cluster_2$superficie)
fit_cluster_2_Gumbel



TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_2_Gumbel,
                            incendios_cluster_2$superficie,
                            year = 100,
                            "plot")



#-------------------------------------------------------------------------------
# cluster_3
summary(incendios_cluster_3$superficie)
fit_cluster_3 <- TFMevt::fitGEV(incendios_cluster_3$superficie, c(0.1,0.1,0.1))
fit_cluster_3


TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         5)
TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         10)
TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         25)
TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         50)
TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         100)

TFMevt::return_level_GEV(fit_cluster_3, 
                         incendios_cluster_3$superficie,
                         100*365, 
                         "plot")



fit <- extRemes::fevd(incendios_cluster_3$superficie)
extRemes::return.level(fit, return.period = 365*5)
TFMevt::mean_excess_GEV(incendios_cluster_3$superficie)



threshold <- 4500

fit_cluster_3_GP <- TFMevt::fitGPD(incendios_cluster_3$superficie, threshold)
fit_cluster_3_GP



TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_3_GP,
                         incendios_cluster_3$superficie,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_3_Gumbel <- TFMevt::fitGumbel(incendios_cluster_3$superficie)
fit_cluster_3_Gumbel
extRemes::fevd(incendios_cluster_3$superficie, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_3_Gumbel,
                            incendios_cluster_3$superficie,
                            year = 100,
                            "plot")





#-------------------------------------------------------------------------------
# cluster_4
max(incendios_cluster_4$superficie)
fit_cluster_4 <- TFMevt::fitGEV(incendios_cluster_4$superficie, c(0.1,0.1,0.1))
fit_cluster_4


TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         5)
TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         10)
TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         25)
TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         50)
TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         100)

TFMevt::return_level_GEV(fit_cluster_4, 
                         incendios_cluster_4$superficie,
                         100*365, 
                         "plot")




TFMevt::mean_excess_GEV(incendios_cluster_4$superficie)



threshold <- 3000

fit_cluster_4_GP <- TFMevt::fitGPD(incendios_cluster_4$superficie, threshold)
fit_cluster_4_GP



TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = threshold,
                         year = 5)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = threshold,
                         year = 10)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = threshold,
                         year = 25)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = threshold,
                         year = 50)
TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = threshold,
                         year = 100)


TFMevt::return_level_GPD(fit_cluster_4_GP,
                         incendios_cluster_4$superficie,
                         threshold = 3,
                         year = 100,
                         "plot")



fit_cluster_4_Gumbel <- TFMevt::fitGumbel(incendios_cluster_4$superficie)
fit_cluster_4_Gumbel
extRemes::fevd(incendios_cluster_4$superficie, type = "Gumbel")




TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 5)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 10)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 25)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 50)

TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 100)


TFMevt::return_level_Gumbel(fit_cluster_4_Gumbel,
                            incendios_cluster_4$superficie,
                            year = 100,
                            "plot")


