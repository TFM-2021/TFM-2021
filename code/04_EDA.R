
library(readr)
library(tidyverse)

aemet_clean <- read_csv("data/data_clean/AEMET/aemet_clean.csv")
View(aemet_clean)

aemet_clean %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum)) +
  facet_wrap(~provincia)

aemet_clean %>%
  filter(provincia=="MURCIA") %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum))
