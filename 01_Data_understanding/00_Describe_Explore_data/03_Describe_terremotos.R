
# VER: https://www.ign.es/web/ign/portal/sis-catalogo-terremotos


library(readr)
library(tidyverse)
library(ggcorrplot)

terremotos_ign <- read_delim("data/data_raw/terremotos-ign.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
View(terremotos_ign)

colnames(terremotos_ign)

#[1] "Evento"       "Fecha"        "Hora"         "Latitud"      "Longitud"    
#[6] "Prof. (Km)"   "Inten."       "Mag."         "Tipo Mag."    "Localización"


# "Tipo Mag." = https://www.ign.es/web/resources/docs/IGNCnig/SIS-Tipo-Magnitud.pdf
# Inten = https://www.ign.es/web/resources/docs/IGNCnig/SIS-Escala-Intensidad-Macrosismica.pdf

# Data volume

dim(terremotos_ign)
dim(terremotos_ign %>% distinct()) # hay varios repetidos


# Free text entries



check_text_entries <- function(dataset, variable) {
  dataset %>%
  select({{variable}}) %>%
  group_by({{variable}}) %>%
  summarise(distintos = n()) %>%
  View()
}


check_text_entries(terremotos_ign,Evento) # no hay repetidos pero sí muchos nulos
  

check_text_entries(terremotos_ign,Latitud) # no hay texto


check_text_entries(terremotos_ign,Longitud)# no hay texto


check_text_entries(terremotos_ign,`Prof. (Km)`)# no hay texto


check_text_entries(terremotos_ign,`Inten.`) # hay nunmeros y numeros romanos


check_text_entries(terremotos_ign,`Mag.`) # no hay texto


check_text_entries(terremotos_ign,`Tipo Mag.`) # no hay texto


check_text_entries(terremotos_ign,`Localización`) # todo texto sin localizaciones claras





# Check attribute types

str(terremotos_ign)

#   Evento = col_double(),
#   Fecha = col_character(),
#   Hora = col_time(format = ""),
#   Latitud = col_double(),
#   Longitud = col_double(),
#   `Prof. (Km)` = col_double(),
#   Inten. = col_character(),
#   Mag. = col_double(),
#   `Tipo Mag.` = col_double(),
#   Localización = col_character()






# Analyze attribute correlations

terremotos_ign_nums <- terremotos_ign[, unlist(lapply(terremotos_ign, is.numeric))]
terremotos_ign_na <- drop_na(terremotos_ign_nums)


ggcorrplot(cor(terremotos_ign_na), hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, title = "corrplot")



# Check attribute value ranges. For each attribute, compute basic statistics


summary(terremotos_ign_nums) # hay negativos en magnitud
  
skimr::skim(terremotos_ign_nums) # prof, mag y tipo mag siguen distribuciones extremas



library(psych)
describe(terremotos_ign_nums)

# Determine if the attribute meaning is used consistently
# la magnitud no puesto que hay varias formas de medirla (ver InT)

