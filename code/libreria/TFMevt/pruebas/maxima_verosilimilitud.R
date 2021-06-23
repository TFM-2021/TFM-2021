GEV_pdf <- function(x){

y=(1/desv)*(((1+E*((x-media)/desv))^(-1/E))^(E+1))*exp(1)^(-((1+E*((x-media)/desv))^(-1/E)))
}

GEV_pdf(1:10,0.2,2,0.9)
E <- 0.9165839
media <- 1.4833406
desv <- 0.5928882

-length(1:10)*log(desv)

library(fitdistrplus)
data("danishmulti")
x<- danishuni$Loss
extRemes::fevd(x)
drop_na(x)
View(danishuni)

X<-(1/desv)*(((1+E*((x-media)/desv))^(-1/E))^(E+1))*exp(1)^(-((1+E*((x-media)/desv))^(-1/E)))
x<-X

extRemes::fevd(x)
(-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv)))     -sum((1+E*((x-media)/desv))^(-1/E)))*-1





1+E*(x-media)/desv
A <- function(x){
  Y=exp(-(1+E*((x-media)/desv))^(-1/E))
}
x
y  <- 1-(media/desv)*(x-media)
sum(y)
-length(x)*log(desv)+ sum((1/E-1)*y - y^(1/E))








