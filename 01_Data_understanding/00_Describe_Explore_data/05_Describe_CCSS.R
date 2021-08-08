

library(readr)
library(tidyverse)
library(ggcorrplot)




expedientes <- read_delim("data/data_raw/CCSS/expedientes.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

indemnizaciones <- read_delim("data/data_raw/CCSS/indemnizaciones.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos <- read_delim("data/data_raw/CCSS/grandes_eventos.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

grandes_eventos_desglosados <- read_delim("data/data_raw/CCSS/grandes_eventos_desglosados.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)

View(grandes_eventos)
View(grandes_eventos_desglosados)

a <- grandes_eventos_desglosados %>% 
  mutate_all(funs(str_replace(., "-",""))) 
unique(a$ID)

# Check data volume, number of multiples, complexity

dim(expedientes)
dim(indemnizaciones)# las mismas 

dim(grandes_eventos)
dim(grandes_eventos_desglosados)

# Note if the data contain free text entries



check_text_entries <- function(dataset, variable) {
  
  dataset %>%
    select({{variable}}) %>%
    group_by({{variable}}) %>%
    summarise(distintos = n()) %>%
    View()
}

  # expedientes

  colnames(expedientes)

  check_text_entries(expedientes, "1997")
  check_text_entries(expedientes, "2005")
  check_text_entries(expedientes, "2010")

  
  # indemnizaciones
  
  colnames(indemnizaciones)
  
  check_text_entries(indemnizaciones, "1997")
  check_text_entries(indemnizaciones, "2005")
  check_text_entries(indemnizaciones, "2010")
  
  # estas dos tablas anteriores están relacionadas
  

  # grandes_eventos
  
  colnames(grandes_eventos)
  
  check_text_entries(grandes_eventos, "Mes y Año de Ocurrencia")
  check_text_entries(grandes_eventos, "Indemnizaciones")
  check_text_entries(grandes_eventos, "Causa del Siniestro*")
  check_text_entries(grandes_eventos, "ID")
  

  # grandes_eventos_desglosados
  
  colnames(grandes_eventos_desglosados)
  
  check_text_entries(grandes_eventos_desglosados, "Nº de Expedientes")
  check_text_entries(grandes_eventos_desglosados, "Indemnizaciones")
  check_text_entries(grandes_eventos_desglosados, "ID")  
  
  
# Check attribute types
  
str(expedientes) # todo charac
str(indemnizaciones)
str(grandes_eventos) # ID debería ser charac
str(grandes_eventos_desglosados)  # ID debería ser charac




# Analyze attribute correlations


expedientes <- expedientes %>% 
  mutate_all(funs(str_replace(., "-","")))%>% 
  mutate_at(vars(-c("PROVINCIA","ID")),as.numeric)

expedientes <- expedientes[, unlist(lapply(expedientes, is.numeric))]
expedientes <- drop_na(expedientes)


ggcorrplot(cor(expedientes), hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE, title = "corrplot")

View(cor(expedientes))
 