

library(skimr)
library(readr)
library(tidyverse)
library(ggcorrplot)

aemet_diarios_join <- read_csv("data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv")


library(dplyr)

dim(aemet_diarios_join %>% distinct())
dim(aemet_diarios_join)
2038831*20
# Identify data and method of capture
# Access data sources-----------------------------------------------------------

# guardado el diccionario de datos

# Use statistical analyses if appropriate---------------------------------------
# Report tables and their relations---------------------------------------------
# Check data volume, number of multiples, complexity-----------------------------

dim(aemet_diarios_join)

# Note if the data contain free text entries------------------------------------

apply(aemet_diarios_join, 2, FUN = unique)
#hay texto en: horaPresMin, horaPresMax, indicativo, provincia, precipitacion

unique(aemet_diarios_join$fecha)
unique(aemet_diarios_join$indicativo)
unique(aemet_diarios_join$provincia)
unique(aemet_diarios_join$altitud)
unique(aemet_diarios_join$prec)


# Check accessibility and availability of attributes ------------------------

# Check attribute types (numeric, symbolic, taxonomy, etc.)--------------------


str(aemet_diarios_join)

#   fecha = col_date(format = ""),
#   indicativo = col_character(),
#   nombre = col_character(),
#   provincia = col_character(),
#   altitud = col_double(),
#   tmed = col_number(),
#   prec = col_character(),
#   tmin = col_character(),
#   horatmin = col_character(),
#   tmax = col_number(),
#   horatmax = col_character(),
#   dir = col_character(),
#   velmedia = col_character(),
#   racha = col_number(),
#   horaracha = col_time(format = ""),
#   sol = col_character(),
#   presMax = col_number(),
#   horaPresMax = col_character(),
#   presMin = col_number(),
#   horaPresMin = col_character()

# hay bastantastes valores char debido a la "," importada del español





# Check attribute value ranges ------------------------------------------------


aemet_diarios_join <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv",
                                dec = ",", sep = ",")



summary(aemet_diarios_join)
skim(aemet_diarios_join)

# Analyze attribute correlations--------------------------------------------


aemet_diarios_join_nums <- aemet_diarios_join[, unlist(lapply(aemet_diarios_join, is.numeric))]
aemet_diarios_join_nums_na <- drop_na(aemet_diarios_join_nums)


ggcorrplot(cor(aemet_diarios_join_nums_na), hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, title = "corrplot")

ggsave(paste0("01_Data_understanding/00_Describe_Explore_data/corrplots/AEMET/corrplot.png"))


# Understand the meaning of each attribute and attribute value in business terms----------------------

colnames(aemet_diarios_join)
#[1] "fecha"       "indicativo"  "nombre"      "provincia"   "altitud"    
#[6] "tmed"        "prec"        "tmin"        "horatmin"    "tmax"       
#[11] "horatmax"    "dir"         "velmedia"    "racha"       "horaracha"  
#[16] "sol"         "presMax"     "horaPresMax" "presMin"     "horaPresMin"




# For each attribute, compute basic statistics (e.g., compute distribution, ----------------------
#average, max, min, standard deviation, variance, mode, skewness, etc.)

library(psych)
describe(aemet_diarios_join) # grouping variable


mode(aemet_diarios_join)

# Decide if the attribute is relevant for the specific data mining goal----------------------

# NO relevantes: sol, dir

# Determine if the attribute meaning is used consistently----------------------


# Interview domain experts to obtain their opinion of attribute relevance----------------------


# Decide if it is necessary to balance the data (based on the modeling techniques to be used)----------------------








