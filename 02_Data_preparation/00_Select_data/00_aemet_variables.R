
library(dplyr)

# Data selection

datos_aemet <- read.csv(file = './data/data_raw/AEMET/data_join_aemet.csv')


View(datos_aemet)

datos <- datos_aemet %>%
  select(-c('glo','ts_20', 'ts_10', 'ts_50', 'X8', 'version', 'prediccion.dia', 
            starts_with('origen'), 'id', 'elaborado', 'nombre', 'indsinop', 'tm_mes', 
            'ta_min','ts_min', 'nt_00', 'np_001', 'np_010', 'hr', 'e', 'evap',
            'w_rec', 'w_med', 'q_med', 'q_min', 'q_mar','nv_0050', 'nv_0100', 'nv_1000',
            'nombre', 'indsinop', 'tm_mes', 'ta_min','ts_min', 'nt_00', 'np_001', 
            'np_010', 'hr', 'e', 'evap', 'w_rec', 'w_med', 'q_med', 'q_min', 
            'q_mar','nv_0050', 'nv_0100', 'nv_1000', 'n_tor', 'n_gra', 'n_llu', 
            'n_nie', 'n_nub', 'p_sol', 'n_des', 'nw_55', 'nw_91', 'n_cub', 'n_fog', 'inso'))  


# 


# Guardamos el dataset con las variables seleccionadas
  
write.csv(datos, file = './02_Data_preparation/00_Select_data/00_aemet_variables.csv')


























