#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples
#' u=rnorm(1000,100,12)
#' normalHist(u);
#' normalHist(u,dens=TRUE)
#' normalHist(u,dens=TRUE,col="lightcyan")
normalHist=function(x,dens=FALSE,...){
  m=mean(x)
  stdev=sd(x)
  xn=seq(min(x),max(x),length=200)
  yn=dnorm(xn,m,stdev)
  maxy=1.1*max(yn)
  hist(x, ylim=c(0,maxy),freq=FALSE,...)
  lines(xn,yn,col="red",lwd=2)
  if (dens) lines(density(x),col="blue",lty=2,lwd=2)
}
