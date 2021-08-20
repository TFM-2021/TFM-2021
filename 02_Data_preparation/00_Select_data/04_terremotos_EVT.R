

library(tidyverse)
library(readr)

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
colnames(terremotos_ign)
View(terremotos_ign)

terremotos_ign %>%
  select(!c(Evento,
            Hora
            ))
