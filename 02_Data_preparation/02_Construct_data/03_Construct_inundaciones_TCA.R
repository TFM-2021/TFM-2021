
library(tidyverse)



inundaciones_TCA_selected <- readRDS(file = "02_Data_preparation/01_Clean_data/inundaciones_TCA_clean.rds")

inundaciones_TCA_selected$meses <- str_split_fixed(inundaciones_TCA_selected$mes_y_ano_de_ocurrencia, " ",4)[,-c(2,4)][,1]
inundaciones_TCA_selected$mes_y_ano_de_ocurrencia <- NULL


meses <- c("Enero",     "Febrero",  "Marzo",  "Abril",  "Mayo",   "Junio",    "Julio","Agosto",
           "Septiembre", "Octubre",    "Noviembre", "Diciembre")

conversion_mes_numero <- data.frame("mes"= 1:12,meses)

inundaciones_TCA_selected <- inner_join(conversion_mes_numero, inundaciones_TCA_selected, by="meses")
inundaciones_TCA_selected$meses <- NULL

inundaciones_TCA_selected$p_sin <- sin((2*pi*inundaciones_TCA_selected$mes)/12) 
inundaciones_TCA_selected$p_cos <- cos((2*pi*inundaciones_TCA_selected$mes)/12) 
inundaciones_TCA_selected$mes <- NULL



inundaciones_TCA_selected$TCA <- NA
inundaciones_TCA_selected$inundacion <- NA
inundaciones_TCA_selected$inundacion_TCA <- NA



for (causa in 1:nrow(inundaciones_TCA_selected)) {
  
  if(str_detect(inundaciones_TCA_selected$causa_del_siniestro[causa],"TCA")){
    
    inundaciones_TCA_selected$TCA[causa] <- 1
    
  }else{
    inundaciones_TCA_selected$TCA[causa] <- 0
  }
  
  if(str_detect(inundaciones_TCA_selected$causa_del_siniestro[causa],"Inundación")){
    
    inundaciones_TCA_selected$inundacion[causa] <- 1
    
  }else{
    inundaciones_TCA_selected$inundacion[causa] <- 0
  }
  
  if (inundaciones_TCA_selected$TCA[causa]+inundaciones_TCA_selected$inundacion[causa] ==2) {
    
    inundaciones_TCA_selected$inundacion_TCA[causa] <- 1
    
  }else{
    
    inundaciones_TCA_selected$inundacion_TCA[causa] <- 0
  }
  
  
}
inundaciones_TCA_selected$inundacion <- NULL

inundaciones_TCA_selected <- inundaciones_TCA_selected %>%
  filter(!str_detect(causa_del_siniestro, "Terremoto|Terrorismo"))





inundaciones_TCA_selected$causa_del_siniestro <- NULL


#library(fastDummies)
#dummy_inundaciones <- dummy_cols(inundaciones_TCA_selected,select_columns = "clase_de_riesgo")




dummy_inundaciones <- inundaciones_TCA_selected



for (causa in 1:nrow(dummy_inundaciones)) {
  
  if(str_detect(dummy_inundaciones$clase_de_riesgo[causa],"Viviendas y Comunidades de Viviendas")){
    
    dummy_inundaciones$viviendas[causa] <- 1
    
  }else{
    dummy_inundaciones$viviendas[causa] <- 0
  }
  
  
  if(str_detect(dummy_inundaciones$clase_de_riesgo[causa],"Comercios")){
    
    dummy_inundaciones$comercios_industria[causa] <- 1
    
  }else{
    dummy_inundaciones$comercios_industria[causa] <- 0
  }
  
  if(str_detect(dummy_inundaciones$clase_de_riesgo[causa],"Industriales")){
    
    dummy_inundaciones$comercios_industria[causa] <- 1
    
  }else{
    dummy_inundaciones$comercios_industria[causa] <- 0
  }
  
  if(str_detect(dummy_inundaciones$clase_de_riesgo[causa],"Vehículos Automóviles")){
    
    dummy_inundaciones$vehiculos[causa] <- 1
    
  }else{
    dummy_inundaciones$vehiculos[causa] <- 0
  }
}







# CREACION COLUMNAS LUGARES-----------------------


for (causa in 1:nrow(dummy_inundaciones)) {
  
  if(str_detect(dummy_inundaciones$lugar_de_ocurrencia[causa],"Generalizado")){
    
    dummy_inundaciones$generalizado[causa] <- 1
    
  }else{
    dummy_inundaciones$generalizado[causa] <- 0
  }
  
  if(str_detect(dummy_inundaciones$lugar_de_ocurrencia[causa],"Sureste Peninsular")){
    
    dummy_inundaciones$sureste_peninsular[causa] <- 1
    
  }else{
    dummy_inundaciones$sureste_peninsular[causa] <- 0
  }
  
  if(str_detect(dummy_inundaciones$lugar_de_ocurrencia[causa],"Cdad. Valenciana")){
    
    dummy_inundaciones$cdad_valenciana[causa] <- 1
    
  }else{
    dummy_inundaciones$cdad_valenciana[causa] <- 0
  }
  
}



dummy_inundaciones$clase_de_riesgo <- NULL
dummy_inundaciones$lugar_de_ocurrencia <- NULL
dummy_inundaciones$`clase_de_riesgo_Daños personales` <- NULL
dummy_inundaciones$causa_del_siniestro <- NULL


quantile(dummy_inundaciones$indemnizaciones)


# DISCRETIZACVION

dummy_inundaciones$indemnizaciones <- arules::discretize(dummy_inundaciones$indemnizaciones,
                                                         breaks = 4,
                                                         infinity = T)


dummy_inundaciones$indemnizaciones <- as.factor(dummy_inundaciones$indemnizaciones)







