

library(readr)
library(tidyverse)

data_clean_aemet <- read_csv("C:/Users/sergi/Desktop/TFM/TFM-2021/data/data_clean/AEMET/data_join_aemet.csv", 
                             col_types = cols(fecha = col_date(format = "%Y-%m")))


columnas <- c(9, 14, 19, 25, 30, 43)


for (fila in 1:nrow(data_clean_aemet)) {
  
  for(columna in columnas){
    
    try(
      if (str_ends(data_clean_aemet[fila,columna],fixed(")"))) {
        
        data_clean_aemet[fila,columna] <- str_split(data_clean_aemet[fila,columna],fixed("("),simplify = T)[1,1]
      }
    ,silent = TRUE)
  }
  
}

write.csv(data_clean_aemet,"../data/data_clean/AEMET/aemet_clean.csv", row.names = FALSE)
