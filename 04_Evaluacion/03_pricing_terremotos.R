

VAL_terremotos_modelo_intensidad <- readRDS(file = "data/data_VAL/VAL_terremotos_modelo_intensidad.rds")

VAL_terremotos_modelo_intensidad$inten <- as.factor(VAL_terremotos_modelo_intensidad$inten)

unique(VAL_terremotos_modelo_intensidad$inten)

