
library(tidyverse)


inundaciones_TCA_selected <- readRDS(file = "02_Data_preparation/01_Clean_data/inundaciones_TCA_clean.rds")

inundaciones_TCA_selected$meses <- str_split_fixed(inundaciones_TCA_selected$mes_y_ano_de_ocurrencia, " ",4)[,-c(2,4)][,1]
inundaciones_TCA_selected$mes_y_ano_de_ocurrencia <- NULL


meses <- c("Enero",     "Febrero",  "Marzo",  "Abril",  "Mayo",   "Junio",    "Julio","Agosto",
           "Septiembre", "Octubre",    "Noviembre", "Diciembre")

conversion_mes_numero <- data.frame("mes"= 1:12,meses)

inundaciones_TCA_selected <- inner_join(conversion_mes_numero, inundaciones_TCA_selected, by="meses")
inundaciones_TCA_selected$meses <- NULL
View(inundaciones_TCA_selected)
