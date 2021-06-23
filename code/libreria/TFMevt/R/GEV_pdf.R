#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param E
#' @param media
#' @param desv
#' @return el histograma con la densidad normal superpuesta
#' @export GEV_pdf

library(ggplot2)

GEV_pdf = function(x, E, media, desv,...){
  
  eq = function(x){
    t <- (1+E*((x-media)/desv))^(-1/E)
    (1/desv)*(t^(E+1))*exp(1)^(-t)
  }
  
  curve(eq, from=min(x), to=max(x), xlab="x", ylab="y",
        main = "Distribución GEV")
    Maxima_verosimilitud <- (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv)))     -sum((1+E*((x-media)/desv))^(-1/E)))*-1
  
  return(print(paste0(" Negative Log-Likelihood Value: ", Maxima_verosimilitud)))
} 


