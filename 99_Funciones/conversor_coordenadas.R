

conversor_coordenadas_decimales <- function(coordenada){
  
  
  horas <- as.numeric(str_sub( coordenada,1,nchar( coordenada)-5))
  
  minutos <- as.numeric(str_sub( coordenada,3,nchar( coordenada)-3))/60
  
  segundos <- as.numeric(str_sub( coordenada,5,nchar( coordenada)-1))/3600
  
  if (str_ends(coordenada, "S")) {
    
    sum(horas,minutos,segundos)*-1
    
  }else if(str_ends(coordenada, "W")){
    
    sum(horas,minutos,segundos)*-1
    
  }else{
    sum(horas,minutos,segundos)
  }
  
  
}