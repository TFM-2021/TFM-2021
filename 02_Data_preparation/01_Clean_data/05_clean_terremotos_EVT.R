
library(tidyverse)


terremotos_EVT_selected <- readRDS("02_Data_preparation/00_Select_data/terremotos_EVT_selected.rds")


terremotos_EVT_selected <- janitor::clean_names(terremotos_EVT_selected)


saveRDS(terremotos_EVT_selected, "data/data_VAL/VAL_terremotos_EVT.rds")
