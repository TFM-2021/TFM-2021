
library(TFMevt)

TFMevt::GEV_pdf()
library(fitdistrplus)

data("danishmulti")
x <- danishuni$Loss
x
x <- danishmulti$Building

eq <- function( par){
  media <- par[1]
  desv <- par[2]
  E <- par[3]
  (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv))) -sum((1+E*((x-media)/desv))^(-1/E)))*-1
}

optim(par = c(0.2,0.3,0.5), fn = eq)
eq(0.2,0.3,0.5)

