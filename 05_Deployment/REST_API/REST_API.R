library(jsonlite)

arbol_intensidad_terremotos <- readRDS("arbol_intensidad_terremotos.rds")


#* @post /prediccion
predict.intensidad <- function(profundidad,
                               magnitud,
                               placa) {
  data <- tibble::tibble(
    "prof_km"=log(as.numeric(profundidad)),
    "inten"=as.factor("."),
    "mag"=(as.numeric(magnitud) - 2.850961)/0.9478287,
    "placa_tectonica"=as.factor(placa))
  
  prediccion <- predict(arbol_intensidad_terremotos, data , type="class")
  return(prediccion)
}