library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(tidyr)

arbol <- read_rds("../shiny-server/arbol_intensidad_terremotos.rds")

matriz_costes <- readRDS("../shiny-server/matriz_costes.rds")


terremotos_evt <- readRDS("../shiny-server/VAL_terremotos_EVT_clusters_clara.rds")



terremotos_evt$fecha <- as.Date(terremotos_evt$fecha, format="%d/%m/%Y")





fitGumbel <- function(x, metodo_optimizacion=NULL){
  
  eq = function(par){
    media <- par[1]
    desv <- par[2]
    
    -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))
    
  }

  optimizacion <- optim(par=c(0.1,0.1), fn = eq, hessian = T, method = "SANN")
}


# Define UI ----
ui <- dashboardPage(
  
  dashboardHeader(title = "CALCULADORA"),
  
  dashboardSidebar(
    
    menuItem("CALCULADORA TERREMOTOS",
             
      tabName = "calculadora terremotos")),
  
  dashboardBody(
    fluidRow(
     tabBox(
       title =  "calculadora_terremotos",
       width = "550px", 
       height = "5000px",
       
      tabPanel("Frecuencia",
               
                box(width = 12,selectInput(
                  "select_cluster",
                  label = "Seleccione cluster",
                  choices = c(1,2,3,4)
                  
                )
                ),
               div(
                 
                 box(width = 50,background = "purple",
                  box(width = 12,background = "navy",
                   h1("Modelo GEV")),
                   box(plotOutput("location_plot"),background = "olive"),
                   
                   box(plotOutput("scale_plot"),background = "olive"),
                   
                   box(plotOutput("shape_plot"),background = "olive"),
                   box(title = "Calidad del ajuste",tableOutput("summary_GEV"),background = "olive", width = 4),
                   box(title = "Desviaciones típicas estimadas",tableOutput("summary_GEV2"),background = "olive", width = 4),
                   box(title = "Matriz covarianzas",tableOutput("summary_GEV3"),background = "olive", width = 4),
                   box(numericInput("return_level_GEV",value = 5,label = "Introduzca el año de cálculo"),
                       tableOutput("calculo_return_GEV"),background = "olive"))
                 ),
               div(
                 
                 box(width = 50,background = "purple",
                 box(width = 12,background = "navy",
                       h1("Modelo GUMBEL")),
                 
                 box(plotOutput("location_plotgumbel"),background = "olive"),
                 
                 box(plotOutput("scale_plotgumbel"),background = "olive"),
                 
                 box(title = "Calidad del ajuste",tableOutput("summary_GUMBEL"),background = "olive", width = 4),
                 box(title = "Desviaciones típicas estimadas",tableOutput("summary_GUMBEL2"),background = "olive", width = 4),
                 box(title = "Matriz covarianzas",tableOutput("summary_GUMBEL3"),background = "olive", width = 4),
                 box(numericInput("return_level_GUMBEL",value = 5,label = "Introduzca el año de cálculo"),
                     tableOutput("calculo_return_GUMBEL"),background = "olive"))
                 
                 )),
       
       
      
       
     tabPanel("Intensidad",
               
          box(valueBoxOutput("pred_inten", width = 12)),
      
          box(solidHeader = TRUE,
              
           sliderInput("magnitud", 
                       label = "Magnitud",
                      min = 0, 
                      max = 10, 
                      value = 5, 
                      step = 0.1)),
          
           box(sliderInput("profundidad", label = "Profundidad",
                      min = 0, 
                      max = 300, 
                      value = 10,
                      step = 0.1)),
          ),
     tabPanel("Coste",
        
        box(valueBoxOutput("pred_coste", width = 12)),
        
        box(numericInput("m2_ladrillo",
                     label = "Metros cuadrados ladrillo",
                     value = 10000,
                     step = 10000)),
        
            box(numericInput("m2_hormigon",
                     label = "Metros cuadrados hormigón",
                     value = 10000,
                     step = 10000)),
        
            box(numericInput("m2_coste",
                     label = "Coste metros cuadrados",
                     value = 1000,
                     step = 10)),
            box( selectInput("intensidad_terremoto",
                    label = "Elija la intensidad",
                    choices = unique(matriz_costes$Terremoto)))
        
        
        
        
      )
    )
  )
)
)
  







# Define server logic ----
server <- function(input, output) {

  fitGEV <-  function(x, parametros_iniciales, metodo_optimizacion=NULL){
    
    eq = function(par){
      media <- par[1]
      desv <- par[2]
      E <- par[3]
      (-length(x)*log(desv)   -(1+1/E)*sum(log(1+E*((x-media)/desv))) -sum((1+E*((x-media)/desv))^(-1/E)))*-1
      
      
    }
    
    optimizacion <- optim(parametros_iniciales,fn = eq,hessian = TRUE,method = metodo_optimizacion)
  }
  
  
  
  
  
  
  
  evt <- reactive({
    
    x <<- terremotos_evt %>%
      filter(cluster == input$select_cluster)%>%
      group_by(fecha)%>%
      summarise(mag = max(mag))%>%
      select(mag)
    
    fitGEV(x$mag, c(0.1,0.1,0.1))
  }) 
  
  
  resultados_fit <- reactive({
    
    tibble("Parametro"= c("location", "scale", "shape"),
           "Valores_optimos"= evt()$par)
  })
  
  valor_location <- reactive({
    as.double(resultados_fit()[1,2])
    
  })
  
  valor_scale <- reactive({
    as.double(resultados_fit()[2,2])
    
  })
  
  valor_shape <- reactive({
    as.double(resultados_fit()[3,2])
    
  })
  
  output$location_plot <- renderPlot({
    

    
    verosimilitud <- c(evt()$value)
    

    # PLOT LOCATION -------------------------------------------------------
    
    # secuencia son los numeros de la variable location para calcular y graficar
    secuencia <-seq(valor_location()-0.1,
                    valor_location()+0.1,
                    0.01)
    
    #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas
    
    verosimilitud_funcion_media <- sapply(secuencia, function(media){
      E <- as.double(resultados_fit()[3,2])
      desv <- as.double(resultados_fit()[2,2])
      
      (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
    })
    
    # agrupamos para plotear
    df <- data.frame(secuencia, verosimilitud_funcion_media)
    
    ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
      geom_line() +
      xlim(as.double(valor_location()-0.1),
           as.double(valor_location()+0.1)) +
      
      labs(title = "Relación Verosimilitud / Location",
           x = "Location",
           y = "Verosimilitud",
           color = NULL) +
      
      geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +
      
      geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
                 color="red",
                 size=3) +
      
      
      theme_minimal()
    
  })
  
  output$scale_plot <- renderPlot({
    
    
    # PLOT SCALE -------------------------------------------------------
    
    # secuencia son los numeros de la variable SCALE para calcular y graficar
    secuencia <-seq(valor_scale()-0.1,
                    valor_scale()+0.1,
                    0.01)
    
    #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas
    
    verosimilitud_funcion_media <- sapply(secuencia, function(desv){
      E <- valor_shape()
      media <- valor_location()
      
      (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
    })
    
    # agrupamos para plotear
    df <- data.frame(secuencia, verosimilitud_funcion_media)
    
    ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
      geom_line() +
      xlim(as.double(valor_scale()-0.1),
           as.double(valor_scale()+0.1)) +
      
      labs(title = "Relación Verosimilitud / Scale",
           x = "Scale",
           y = "Verosimilitud",
           color = NULL) +
      
      geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +
      
      geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
                 color="red",
                 size=3) +
      
      theme_minimal()
  })
  
  output$shape_plot <- renderPlot({
    
    

    
    # secuencia son los numeros de la variable SHAPE para calcular y graficar
    secuencia <-seq(valor_shape()-0.1,
                    valor_shape()+0.1,
                    0.01)
    
    #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas
    
    verosimilitud_funcion_media <- sapply(secuencia, function(E){
      media <- valor_location()
      desv <- valor_scale()
      
      (-length(x)*log(desv)-(1+1/E)*sum(log(1+E*((x-media)/desv)))-sum((1+E*((x-media)/desv))^(-1/E)))*-1
    })
    
    # agrupamos para plotear
    df <- data.frame(secuencia, verosimilitud_funcion_media)
    
    ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
      geom_line() +
      xlim(as.double(valor_shape()-0.1),
           as.double(valor_shape()+0.1)) +
      
      labs(title = "Relación Verosimilitud / Shape",
           x = "Scale",
           y = "Verosimilitud",
           color = NULL) +
      
      geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +
      
      geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
                 color="red",
                 size=3) +
      theme_minimal()
  })
  
  
  output$summary_GEV <- renderTable({
    verosimilitud <- c( evt()$value)
    
    
    tibble("Negative log likelihood"=verosimilitud,
           
           "AIC"= 2*3+2*verosimilitud,
           
           
           "BIC"=2*verosimilitud+3*log(length(x)))
    
    
  })
  
  output$summary_GEV2 <- renderTable({
    
    data.frame("location"=sqrt(solve(evt()$hessian)[1,1]),
               "scale"=sqrt(solve(evt()$hessian)[2,2]),
               "shape"=sqrt(solve(evt()$hessian)[3,3]))
    
  })
  
  output$summary_GEV3 <- renderTable({
    
    data.frame("location"=solve(evt()$hessian)[1,],
               "scale"=solve(evt()$hessian)[2,],
               "shape"=solve(evt()$hessian)[3,])
    
  })
  

  
  output$calculo_return_GEV <- renderTable({
    
    resultados_fit <- tibble("Parametro"= c("location", "scale", "shape"),
                             "Valores_optimos"= evt()$par)
    
    
    V <- as.matrix(tibble("location"=solve(evt()$hessian)[1,],
                          "scale"=solve(evt()$hessian)[2,],
                          "shape"=solve(evt()$hessian)[3,]))
    
    location <- as.double(resultados_fit[1,2])
    scale <-  as.double(resultados_fit[2,2])
    shape <-  as.double(resultados_fit[3,2])
    
    p <- 1/(input$return_level_GEV*365)
    
    
    y_p <- -log(1-p)
    
    
    Z <- location - scale/shape*(1-(-log(1-p))^(-shape))
    
    d_2 <- as.numeric( -shape^(-1)*(1-y_p^(-shape)))
    
    d_3 <- as.numeric(scale*shape^(-2)*(1-y_p^(-shape)) -scale*shape^(-1)*y_p^(-shape)*log(y_p))
    
    z_T_p <- c(1, d_2 , d_3)
    
    Var <- t(matrix(z_T_p)) %*% V %*% matrix(z_T_p)
    
    tibble("Limite inferior: "= Z +1.96 * as.numeric(sqrt(Var)),
           "Media: "= Z,
           "Limite superior: "= Z -1.96 * as.numeric(sqrt(Var)))
    
  })
  
  
  # GUMBEL----------------------------------------------------------------------
  
  evt_gumbel <- reactive({
  
    x <<- terremotos_evt %>%
      filter(cluster == input$select_cluster)%>%
      group_by(fecha)%>%
      summarise(mag = max(mag))%>%
      select(mag)
    
    fitGumbel(x$mag)
  })
  
  
  output$location_plotgumbel <- renderPlot({
    
    resultados_fit <- tibble("Parametro"= c("location", "scale"),
                             "Valores_optimos"= evt_gumbel()$par)
    
    verosimilitud <<- c( evt_gumbel()$value)
    
    valor_location <- as.double(resultados_fit[1,2])
    valor_scale <-  as.double(resultados_fit[2,2])
    
    
    # PLOT LOCATION -------------------------------------------------------
    
    # secuencia son los numeros de la variable location para calcular y graficar
    secuencia <-seq(valor_location-0.1,
                    valor_location+0.1,
                    0.01)
    
    #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas
    
    verosimilitud_funcion_media <- sapply(secuencia, function(media){
      desv <- as.double(resultados_fit[2,2])
      
      -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))
    })
    
    # agrupamos para plotear
    df <- data.frame(secuencia, verosimilitud_funcion_media)
    
    ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
      geom_line() +
      xlim(as.double(valor_location-0.1),
           as.double(valor_location+0.1)) +
      
      labs(title = "Relación Verosimilitud / Location",
           x = "Location",
           y = "Verosimilitud",
           color = NULL) +
      
      geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +
      
      geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
                 color="red",
                 size=3) +
      theme_minimal()
    
    
  })
  
  output$scale_plotgumbel <- renderPlot({
    
    resultados_fit <- tibble("Parametro"= c("location", "scale"),
                             "Valores_optimos"= evt_gumbel()$par)
    
    verosimilitud <<- c( evt_gumbel()$value)
    
    valor_location <- as.double(resultados_fit[1,2])
    valor_scale <-  as.double(resultados_fit[2,2])
    
    # secuencia son los numeros de la variable SCALE para calcular y graficar
    secuencia <-seq(valor_scale-0.1,
                    valor_scale+0.1,
                    0.01)
    
    #  aplicaicon de la funcion de versimilutd con las otras dos variables fijas
    
    verosimilitud_funcion_media <- sapply(secuencia, function(desv){
      media <- valor_location
      
      -(-length(x)*log(desv)-sum((x-media)/desv)-sum(exp(-(x-media)/desv)))
    })
    
    # agrupamos para plotear
    df <- data.frame(secuencia, verosimilitud_funcion_media)
    
   ggplot(df,aes(secuencia, verosimilitud_funcion_media))+
      geom_line() +
      xlim(as.double(valor_scale-0.1),
           as.double(valor_scale+0.1)) +
      
      labs(title = "Relación Verosimilitud / Scale",
           x = "Scale",
           y = "Verosimilitud",
           color = NULL) +
      
      geom_hline(yintercept=min(verosimilitud_funcion_media), color="red") +
      
      geom_point(data = df[which.min(df$verosimilitud_funcion_media), ],
                 color="red",
                 size=3) +
      theme_minimal()
    
    
    
  })
  

  output$summary_GUMBEL <- renderTable({
    
    
    
    verosimilitud <- c( evt_gumbel()$value)
    
    
    tibble("Negative log likelihood"=verosimilitud,
           
           "AIC"= 2*2+2*verosimilitud,
           
           
           "BIC"=2*verosimilitud+3*log(length(x)))
    
    
  })
  
  
  output$summary_GUMBEL2 <- renderTable({
    
    data.frame("location"=sqrt(solve(evt_gumbel()$hessian)[1,1]),
               "scale"=sqrt(solve(evt_gumbel()$hessian)[2,2]))
    
  })
  
  
  output$summary_GUMBEL3 <- renderTable({
    
    data.frame("location"=solve(evt_gumbel()$hessian)[1,],
               "scale"=solve(evt_gumbel()$hessian)[2,])
    
  })
  
  
  
  output$calculo_return_GUMBEL <- renderTable({
    
    resultados_fit <- tibble("Parametro"= c("location", "scale"),
                             "Valores_optimos"= evt_gumbel()$par)
    
    location <- as.double(resultados_fit[1,2])
    scale <-  as.double(resultados_fit[2,2])


    p <- 1/(input$return_level_GUMBEL*365)
    
      
      Z <- as.numeric(location-scale*log(-log(1-p)))
      
      
      
      tibble(media=Z)
    
  })
  
  
  
  
  
  
  
  
  
  
  output$pred_inten <- renderValueBox({ 
    
    
    prediction <- predict(
      arbol,
      tibble("prof_km"=log(input$profundidad),
             "inten"=as.factor("."),
             "mag"=(input$magnitud - 2.850961)/0.9478287,
             "placa_tectonica"=as.factor(0)),
      type = "class")
    
    
    prediction_prob <- as_tibble(predict(
      arbol,
      tibble("prof_km"=log(input$profundidad),
             "inten"=as.factor("."),
             "mag"=(input$magnitud - 2.850961)/0.9478287,
             "placa_tectonica"=as.factor(0)),
      type = "prob"))%>%
      gather()%>% 
      arrange(desc(value)) %>% 
      slice(1) %>% 
      select(value)
    
    
    valueBox(
      subtitle = paste0(round(100*as.numeric(prediction_prob), 0), "% de probabilidad"),
      value = paste0("Intensidad: ",as.character(prediction)),
    )
    
  }) %>%
    bindCache(input$profundidad,
              input$magnitud)
  
  
  
  output$pred_coste <- renderValueBox({
    
    millones_m2_ladrillo <- input$m2_ladrillo
    millones_m2_hormigon <- input$m2_hormigon
    precio_m2 <- input$m2_coste
    
    matriz_filtrada <- matriz_costes %>%
      filter(Terremoto ==  input$intensidad_terremoto)
    
    
    coste_ladrillos <- matriz_filtrada$`Ladrillo y madera` * millones_m2_ladrillo*precio_m2 * matriz_filtrada$`Percentages of repair to reposition`
    coste_hormigón <- matriz_filtrada$`Hormigón armado` * millones_m2_hormigon*precio_m2 * matriz_filtrada$`Percentages of repair to reposition`
    
    
    
    
    valueBox(
      value = paste0((coste_ladrillos + coste_hormigón)/1000000, " millones €"),
      subtitle = paste0("Coste del terremoto: "),
    )
    
  })
  
  
  
}




# Run the app ----
shinyApp(ui = ui, server = server)













