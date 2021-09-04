
datos1 <- read.csv('aemet_1.csv')
datos1 <- datos1[,-1]


datos2 <- read.csv('aemet_2.csv') 
datos2 <- datos2[,-1]


datos3 <- read.csv('aemet_3.csv')
datos3 <- datos3[,-1]

datos1_2 <- rbind(datos1,datos2) 

datos_aemet_diarios <- rbind(datos1_2, datos3)

write.csv(datos_aemet_diarios, 'datos_aemet_diarios.csv')






