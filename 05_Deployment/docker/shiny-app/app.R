library(shiny)
library(dplyr)
library(tidyr)
library(shinydashboard)


arbol <- readRDS("../shiny-server/arbol_intensidad_terremotos.rds")


matriz_costes <- readRDS("../shiny-server/matriz_costes.rds")

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = "CALCULADORA"),
  dashboardSidebar(
    menuItem(
      "CALCULADORA TERREMOTOS",
      tabName = "calculadora_terremotos"
    )
  ),
  dashboardBody(
    tabItem(
      
      tabName = "calculadora_terremotos",
      
      div(
        box(valueBoxOutput("pred_inten", width = 12)),
        
        box(solidHeader = TRUE,
            sliderInput("magnitud", label = "Magnitud",
                        min = 0, max = 10, value = 5, step = 0.1)),
        box(sliderInput("profundidad", label = "Profundidad",
                        min = 0, max = 300, value = 10,step = 0.1)),
      ),
      div(
        
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



# Define server logic ----
server <- function(input, output) {
  
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
      value = paste0(round(100*as.numeric(prediction_prob), 0), "%"),
      subtitle = paste0("Intensidad: ",as.character(prediction)),
    )
    
  })
  
  
  
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















