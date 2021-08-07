library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(corrplot)

# Datos

datos_aemet <- read.csv(file = './data/data_raw/AEMET/data_join_aemet.csv')

View(datos_aemet)

colnames(datos_aemet)


nrow(datos_aemet) # 99780 filas
ncol(datos_aemet) # 63 variables

attach(datos_aemet)

# detach()

# summary

summary(datos_aemet)




# Nulos 


Valores_nulos <- datos_aemet %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 


Valores_nulos

# Visualizacion de los nulos

missing.values <- datos_aemet %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Total de los datos", "Valores Perdidos")) +
  coord_flip() +
  labs(title = "Porcentaje de valores nulos", x =
         'Variable', y = "% de valores perdidos")

# Porcentaje sobre el total de datos de la varaible
percentage.plot

# En filas 
row.plot <- datos_aemet %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Total de los datos", "Valores Perdidos")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Número de fila", title = "Valores nulos en las filas") +
  coord_flip()

row.plot


# Correlacion


datos_numericos <- datos_incendios[,c(2,11,13,16,17,18,19,21)]

datos_numericos <- datos_numericos %>% drop_na()


summary(datos_numericos)


datos.cor <- cor(datos_numericos)
corrplot(datos.cor)






# Variables por separado









# Mapas





















