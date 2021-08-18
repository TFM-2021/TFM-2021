
library(tidyverse)

inundaciones_TCA_selected <- readRDS("02_Data_preparation/00_Select_data/inundaciones_TCA_selected.rds")


View(inundaciones_TCA_selected)


inundaciones_TCA_selected[inundaciones_TCA_selected=="-"] <- "0"



inundaciones_TCA_selected <- inundaciones_TCA_selected %>%
  mutate_all(funs(str_replace(.,",","")))%>%
  mutate_all(funs(str_replace(.,",","")))%>%
  mutate(Indemnizaciones = as.numeric(Indemnizaciones.x))%>%
  select(!Indemnizaciones.x)




inundaciones_TCA_selected <- janitor::clean_names(inundaciones_TCA_selected)

saveRDS(inundaciones_TCA_selected, file = "02_Data_preparation/01_Clean_data/inundaciones_TCA_clean.rds")











