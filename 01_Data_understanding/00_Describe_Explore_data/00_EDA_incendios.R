library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(corrplot)

# Datos

datos_incendios <- read.csv(file = './data/data_raw/incendios.csv')


View(datos_incendios)

colnames(datos_incendios)

# Summary

summary(datos_incendios)

# Los id son de tipo numérico, habrá que pasarlos a tipo charachter.
# Hay 18 comunidades autonomas que habra que encontrar el significado y 999 municipios
# causa supuesta es siempre 1 salvo por nulos
# En muertos parece que son muchos los nulos, heridos tambien, gastos tambien


# Nulos

## Tabla con los nulos

Valores_nulos <- datos_incendios %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 


# key            num.missing
# <chr>                <int>
# 1 muertos              79916
# 2 heridos              79569
# 3 gastos               71016
# 4 perdidas             48291
# 5 causa_supuesta       36175
# 6 lat                     24
# 7 lng                     24


# Visualizacion de los nulos

missing.values <- datos_incendios %>%
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
row.plot <- datos_incendios %>%
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

# Distribución de las causas

unique(datos_incendios$causa)

ggplot(data = datos_incendios) + 
  geom_bar(mapping = aes(x = causa), fill = 'lightblue')


datos_incendios %>% count(causa)

# causa     n
# 1     1  1895
# 2     2 14636
# 3     3  4000
# 4     4 49603
# 5     5 11256
# 6     6  1250


## correlaciones

datos_numericos <- datos_incendios[,c(2,11,13,16,17,18,19,21)]

datos_numericos <- datos_numericos %>% drop_na()


summary(datos_numericos)


datos.cor <- cor(datos_numericos)
corrplot(datos.cor)

# Histograma de la superficie


ggplot(datos_incendios, aes(superficie)) +
  geom_histogram()

attach(datos_incendios)

max(superficie)  

mean(superficie)


# Histogramas de control de extincion


ggplot(datos_incendios, aes(time_ctrl)) +
  geom_histogram()

mean(time_ctrl)


ggplot(datos_incendios, aes(time_ext)) +
  geom_histogram()


# Causa desc ?

ggplot(data = datos_incendios) + 
  geom_bar(mapping = aes(x = causa_desc), fill = 'lightblue')

datos_incendios %>% 
  count(causa_desc) %>% 
  arrange(desc(n))

unique(causa_desc)



# ? Mapas


















