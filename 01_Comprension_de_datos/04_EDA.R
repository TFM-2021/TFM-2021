
library(readr)
library(tidyverse)

aemet_clean <- read_csv("data/data_clean/AEMET/aemet_clean.csv")
View(aemet_clean)

aemet_clean %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum)) +
  facet_wrap(~provincia)

aemet_clean %>%
  filter(provincia=="MURCIA") %>%
  group_by(fecha, provincia) %>%
  summarise(sum = sum(p_max)) %>%
  ggplot()+
  geom_line(aes(fecha, sum))


attach(aemet_clean)  

library(gridExtra)
Buscar_nulos <- function(data_set,cols){
  
  
  p <-lapply(cols, function(vars) {
    
    nulos <- sum(is.na(data_set[,vars]))/nrow(data_set)*100
    
    data_set %>%
      
      select(fecha, "variable"= {{vars}}) %>%
      filter(is.na(variable)) %>%
      group_by(fecha) %>%
      summarise(missings = n())%>%
      
      ggplot(aes_string()) +
      
      geom_col(aes(fecha, missings/nrow(data_set)*100),
               color="darkgreen")+
      
      labs(title = paste0("Distribución de NAs por ", vars),
           subtitle = paste0("Porcentaje nulos: ",nulos," %"),
           x = NULL,
           y = "Porcentaje sobre la columna")+
      
      theme_light()
    

  })
  
  ggsave(
    filename = "plots.pdf", 
    plot = marrangeGrob(p, nrow=1, ncol=1), 
    width = 15, height = 9
  )

}


Buscar_nulos(aemet_clean, colnames(aemet_clean[,names(aemet_clean) != "fecha"]) )
aemet_clean$w_rec
aes_string("w_rec")
colnames(aemet_clean[,-c(fecha)])

attach(aemet_clean)







