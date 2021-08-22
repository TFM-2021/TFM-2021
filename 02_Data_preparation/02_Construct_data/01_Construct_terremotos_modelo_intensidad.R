library(tidyverse)

terremotos_modelo_intensidad <- readRDS(file = "02_Data_preparation/01_Clean_data/terremotos_clean_modelo_intensidad.rds")


terremotos_modelo_intensidad <- terremotos_modelo_intensidad %>%
  dplyr::mutate(inten = replace(inten, inten == "I", "<IV"),
                inten = replace(inten, inten == "I-II", "<IV"),
                inten = replace(inten, inten == "II", "<IV"),
                inten = replace(inten, inten == "II-III", "<IV"),
                inten = replace(inten, inten == "III", "<IV"),
                inten = replace(inten, inten == "III-IV", "IV"),
                inten = replace(inten, inten == "IV-V", "V"))


terremotos_modelo_intensidad$inten <- as.factor(terremotos_modelo_intensidad$inten)


saveRDS(terremotos_modelo_intensidad, "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")

