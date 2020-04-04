rm(list=ls())
#install.packages("shiny")
library(shiny)
#install.packages("deSolve")
library(deSolve)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)

options(scipen=999)


# Define UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML("shiny-text-output{ background: #fff; }"))),
  h2("Modelo Epidemiologico Compartimental COVID19",
     style = "font-family: 'Arial';
     color: #fff; text-align: center;
     background-image: url('texturebg.png');
     padding: 8px"),
  titlePanel("Parámetros del modelo"),
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("beta", "Prob. Contagio",  
                  min = 0, max = 0.1, value = 0.001, step = 0.0001),
      sliderInput("sigma", "Prob. Asintomatico",  
                  min = 0, max = 1, value =1/2.34 , step = 0.001),
      sliderInput("gamma", "Prob. Infectado",  
                  min = 0, max = 1, value = 0.34965034965035, step = 0.001),
      sliderInput("mu", "Prob. Hospitalización",  
                  min = 0, max = 1, value = 1/3.2, step = 0.001),
      sliderInput("uci", "Prob. UTI",  
                  min = 0, max = 1, value = 0.05, step = 0.001),
      sliderInput("alfa", "Prob. de muerte pacientes UTI",  
                  min = 0, max = 1, value = 0.42, step = 0.001),
      sliderInput("heta", "Días de UTI - fallecidos",  
                  min = 0, max = 20, value = 7, step = 1),
      sliderInput("xi", "Días de UTI - sobrevivientes",  
                  min = 0, max = 20, value = 10, step = 1),
      sliderInput("camas", "Camas hospitalarias",  
                  min = 0, max = 5000, value = 500)
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Evolución de la pandemia",
                 fluidRow(
                   column(12, h2("Evolución de la pandemia", align="center"),
                          h5("Cargue en el espacio que se encuentra debajo del gráfico
                             los datos correspondientes a la situación actual de la 
                             pandemia en el territorio a analizar. Seleccione, además, las características
                             que desea observar en el gráfico (infectados, recuperados, pacientes en UTI, etc."),
                          plotOutput("distPlot"))),
                 fluidRow(
                   column(12, h2("Datos de la situación actual", align="center"),
                          fluidRow(
                            column(3, checkboxGroupInput("lineas", "Conceptos a visualizar",
                                                         choices = c("Suceptibles" = 1, "Expuestos" = 2, "Asintomaticos" = 3,
                                                                     "Infectados" = 4,"UTI" = 5, "Defunciones" = 6,
                                                                     "Recuperados" = 7,"Camas" = 8), selected = c(4,5,8))),
                            
                            column(9, ("Condiciones iniciales"),
                                   fluidRow(
                                     column(6,
                                            column(12, h6(numericInput("S", h6("Suceptibles"),value = "1500000"))),
                                            column(12, h6(numericInput("I", h6("Infectados"),value = "100"))),
                                            column(12, h6(numericInput("R", h6("Recuperados"),value = "7")))
                                     ),
                                     
                                     column(6,
                                            column(12, h6(numericInput("UTI", h6("UTI"),value = "12"))),
                                            column(12, h6(numericInput("D", h6("Defunciones"),value = "1"))),
                                            column(12, h6(numericInput("t", h6("Días a visualizar"),value = "30")))
                                     )
                                   ) 
                            )
                          )
                   )
                 )
                 
                 
                   ),

        tabPanel("Estructura del modelo",
                 fluidRow(
                   column(12, h2("Estructura del modelo", align="center"),
                          h5("El modelo aplicado es consistente con el desarrollo de Arenas, et al. (2020).
                             Se trata de un modelo determinístico de tipo compartimental de propagación de epidemias, 
                             típicamente conocido en la bibliografía como SIR o SEIR, con las siguientes modificaciones:" ),
                          a("acceder al estudio", href="https://www.medrxiv.org/content/10.1101/2020.03.21.20040022v1"),
                          img(src="modelo_compartimental.png"),
                          h5("La estructura del modelo corre internamente en la aplicación. El objetivo es que el personal
                             especializado, en base a su conocimiento experto y la experiencia de campo pueda fijar los
                             parámetros necesarios para lograr una adecuada predicción de la evolución de la pandemia
                             en la Provincia de Córdoba. En tal sentido, la aplicación propone una interfaz que permite
                             a las autoridades modificar los parámetros y analizar los resultados del modelo predictivo de forma inmediata." ), 
                          h5("Se prevé, además, la vinculación de las estimaciones del modelo con la evolución espacio-temporal
                             de la pandemia. A tal fin, se desarrolla en la aplicación la posibilidad de visualizar un mapa 
                             de riesgo en todo el territorio provincial. Sin embargo, para lograr un desarrollo adecuado de
                             esta herramienta es necesaria la interacción con la autoridades, a los fines de definir los 
                             parámetros de mobilidad de la población que permitan una correcta estimación territorial." ) ,
                          h1(),
                          h1(),
                          h4("Equipo de trabajo:" ) ,
                          h5("Mgter. Juan Pablo Carranza" ),
                          h5("Mgter. Federico Monzani" ),
                          h5("Lic. Emilia Bullano" ),
                          h5("Lic. Rocío Cerino" ),
                          h5("Lic. Fernando Pronello" ))))
        
        
                   )
        )
    
                          )
                   )

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # Se crea el DataFrame
    datos <- data.frame(t=as.numeric(0:input$t), S=as.numeric(input$S), E=as.numeric(input$I), 
                        A=as.numeric(input$I), I=as.numeric(input$I), H=as.numeric(input$UTI),
                        D=as.numeric(input$D), R=as.numeric(input$R))
    #View(datos)
    #plot(datos$S)
    # Loops (armado del modelo)
    for(i in 1:length(datos$t)){
      datos$S[i] <- datos$S[1] * (1 - input$beta) ^ (i-1)
    }
    for(i in 2:length(datos$t)){
      datos$E[i] <- datos$S[i-1]*input$beta + (datos$E[i-1]*(1 - input$sigma))
    }
    for(i in 2:length(datos$t)){
      datos$A[i] <- datos$E[i-1]*input$sigma + datos$A[i-1]*(1-input$gamma)
    }
    for(i in 2:length(datos$t)){
      datos$I[i] <- datos$A[i-1]*input$gamma + datos$I[i-1]*(1-input$mu)
    }
    for(i in 2:length(datos$t)){
      datos$H[i] <- datos$I[i-1]*input$mu*input$uci + datos$H[i-1]*input$alfa*(1-(1/input$heta)) + datos$H[i-1]*(1-input$alfa)*(1-(1/input$xi))
    }
    for(i in 2:length(datos$t)){
      datos$D[i] <- datos$H[i-1]*input$alfa*(1/input$heta) + datos$D[i-1]
    }  
    for(i in 2:length(datos$t)){
      datos$R[i] <- datos$I[i-1]*(1-input$uci)*input$mu + datos$H[i-1]*(1-input$alfa)*(1/input$xi) + datos$R[i-1]
    }
    datos$Camas = input$camas
    
    lineas <- ifelse(input$lineas==1,"Suceptibles",
                     ifelse(input$lineas==2,"Expuestos",
                            ifelse(input$lineas==3,"Asintomaticos",
                                   ifelse(input$lineas==4,"Infectados",
                                          ifelse(input$lineas==5,"UTI",
                                                 ifelse(input$lineas==6,"Defunciones",
                                                        ifelse(input$lineas==7,"Recuperados","Camas")))))))
    
    lineas = as.character(lineas)
    lineas = c(lineas,"t")
    datos_filtrado = datos %>% 
      rename(
        Suceptibles = S,
        Expuestos = E,
        Asintomaticos=A,
        Infectados=I,
        UTI=H,
        Defunciones=D,
        Recuperados=R
      )
    datos_filtrado = subset(datos_filtrado, select = (lineas))
    datos_filtrado = melt(datos_filtrado,id=c("t"))
    
    ggplot(data=datos_filtrado,
           aes(x=t, y=value, colour=variable)) +
      labs(y="Población", x = "Días") +
      theme_classic() + labs(colour = "Conceptos") +
      theme(legend.position = c(0.8, 0.8)) +
      ylim(0,max(datos_filtrado$value)*1.05)+
      scale_colour_manual(values = c("Suceptibles" = "darkgreen",
                                     "Expuestos" = "red",
                                     "Asintomaticos" = "orange",
                                     "Infectados" = "darkblue",
                                     "UTI" = "black",
                                     "Defunciones" = "lightblue",
                                     "Recuperados" = "green",
                                     "Camas" = "black")) + 
      geom_line()
    
  })

} 

shinyApp(ui = ui, server = server)



