

library(readr)
library(tidyverse)
library(ggcorrplot)


terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                             trim_ws = TRUE)

catalogoComunSV_1628416785167 <- read_delim("data/data_raw/catalogoComunSV_1628416785167.csv", 
                                            ";", escape_double = FALSE, trim_ws = TRUE)
View(catalogoComunSV_1628416785167)
terremotos_ign %>%
  filter(Mag. > 5) %>%
  ggplot()+
  geom_jitter(aes(Fecha, Mag.))+
  geom_smooth(aes(Fecha, Mag.))


terremotos_ign %>%
  filter(Mag. > 5) %>%
  ggplot()+
  geom_density(aes(Mag.))


