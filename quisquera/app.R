library(shiny)
library(shinythemes) # Para cambiarle el tema de colores.
library(tidyverse)

# Interfaz de usuario
ui <- navbarPage(
    titlePanel ( "Muestreo de aceptación de lotes"), # Application title
    theme = shinytheme("darkly"), # Tema de colores! https://gallery.shinyapps.io/117-shinythemes/
    tabPanel( "Curva caracterisitca OC",
        numericInput(inputId="nsamp",
                        label="Tamaño de muestra",
                        min = 10,
                        max = 500,
                        value=200),
        sliderInput(inputId = "aceptance",
                        label = "Criterio de aceptación",
                        min= 0,
                        max = 10,
                        value = 1),
        plotOutput("Plot") # Aca va el grafico
        ), # Cierro primer Tab
    tabPanel ("Curva OC con riesgos",
        numericInput(inputId="nsamp2",
                         label="Tamaño de muestra",
                         min = 10,
                         max = 500,
                         value=200),
            sliderInput(inputId = "aceptance2",
                        label = "Criterio de aceptación",
                        min= 0,
                        max = 10,
                        value = 1),
            numericInput(inputId="pfail",
                         label="Propoción de fallas",
                         min = 0,
                         max = 0.02,
                         value=0),
            plotOutput("Plot2") # Aca va el grafico
        ), # Cierro el segundo Tab
    tabPanel ("Curva AOQ",
              numericInput(inputId="nlote",
                           label="Tamaño del Lote",
                           min = 500,
                           max = 10000,
                           value=6000),
              numericInput(inputId="nsamp3",
                           label="Tamaño de muestra",
                           min = 10,
                           max = 500,
                           value=200),
              sliderInput(inputId = "aceptance3",
                          label = "Criterio de aceptación",
                          min= 0,
                          max = 10,
                          value = 1),
              plotOutput("Plot3") # Aca va el grafico
    ) # Cierro el segundo Tab
) # Cierro NavbarPage


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- renderPlot({
        # generate Pa based on input$samplesize and input$d from ui.R
        nsamp <- input$nsamp
        aceptance <- input$aceptance
        p <- seq(0 , 0.1, by = 0.001)
        oc_data<-tibble(p=p)
        oc_data <- oc_data %>% 
            mutate(Pa =  pbinom(q =aceptance , size = nsamp, prob = p))
        
       # draw the OC with specified number of n and d
        ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
            labs(title = paste("Curva OC para el plan:", nsamp, "-", aceptance,"/",aceptance+1),
                 x = "Fracción defectuosas en el lote",
                 y = "Probabilidad de aceptación") + 
            theme_minimal() + theme(text=element_text(size=15,face="bold")) + 
            scale_x_continuous(limits=c(0,0.04))
           
    })
    output$Plot2 <- renderPlot({
        # generate Pa based on input$samplesize and input$d from ui.R
        nsamp <- input$nsamp2
        aceptance <- input$aceptance2
        pfail <- input$pfail
        
         p <- seq(0 , 0.1, by = 0.001)
        oc_data <-tibble(p=p)
        
        oc_data <- oc_data %>% 
            mutate(Pa =  pbinom(q = aceptance , size = nsamp, prob = p))
        
        defectos <- tibble (pfail)
        
        defectos <- defectos %>% 
            mutate(pa2 = pbinom(q =aceptance , size = nsamp, prob = pfail))
        
       # draw the OC with specified number of n and d
        ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
            labs(title = paste("Curva OC para el plan:", nsamp, "-", aceptance,"/",aceptance+1),
                 x = "Fracción defectuosas en el lote",
                 y = "Probabilidad de aceptación") + 
            theme_minimal() + theme(text=element_text(size=15,face="bold")) + 
            scale_x_continuous(limits=c(0,0.04)) +
            geom_segment(data = defectos, aes(x = pfail, y = 0, xend = pfail, yend = pa2),color="red",linetype="dashed",size=2) +
            geom_segment(data = defectos, aes(x = 0, y = pa2, xend = pfail, yend = pa2),color="red",linetype="dashed", size=2) + 
            annotate("text", x = 0.001, y = (defectos$pa2+0.05), label = paste(round((defectos$pa2*100),2),"%"))
        
    })
    output$Plot3 <- renderPlot({
        # generate Pa based on input$samplesize and input$d from ui.R
        nsamp <- input$nsamp3
        aceptance <- input$aceptance3
        nlote <- input$nlote
        
        p <- seq(0 , 0.1, by = 0.001)
        
        aoq_data<- tibble(p) %>% 
            mutate (Pa = pbinom(q = aceptance, size = nsamp, prob = p),
                    Aoq =  (Pa * p * (nlote-nsamp)) / nlote)
        
        ggplot(aoq_data, aes(y=Aoq , x=p)) + geom_line( size = 2) +  
            labs(title = "Curva AOQ para el plan: 200-1/2",
                 x = "Fracción defectuosas en el lote",
                 y = "Fracción defectuosa promedio de salida (AOQ)") + 
            theme_classic() + theme(text=element_text(size=12,face="bold")) + 
            scale_x_continuous(limits=c(0,0.04))      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
