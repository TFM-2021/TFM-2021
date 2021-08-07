
library(tidyverse)

aemet_diarios_join <- read.csv2("data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv",
                                dec = ",", sep = ",",fileEncoding = "utf-8")


aemet_diarios_join_nums <- aemet_diarios_join[, unlist(lapply(aemet_diarios_join, is.numeric))]



aemet_diarios_join[aemet_diarios_join$prec == "Ip",] <- 0.1

aemet_diarios_join %>%
  select(prec) %>%
  str_replace(",",".")

for(provincia_ in unique(aemet_diarios_join_nums$provincia)){
  
  
  df_filter <- aemet_diarios_join_nums[aemet_diarios_join_nums$provincia == provincia_,]
  
  
  a <- df_filter %>%
    group_by(fecha) %>%
    summarise(across(everything(), list(mean)))  
  
  
  for (variable in colnames(a)) {
    
    ggplot(a)+
      geom_density(aes({variable}),color="red")
  }
  
  
  
}










