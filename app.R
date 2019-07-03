#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("App filas"),
   
   # Sidebar with a slider input for number of bins 
    # sidebarLayout(
     #  sidebarPanel(
      #   numericInput(inputId = "num",
       #               label = "Número  de observações",
        #              value = 100)
         
       #),
      # mainPanel(plotOutput(outputId = "hist"))
     #),
   navbarPage(title="Filas",
              tabPanel("Descrição",
              tabsetPanel(tabPanel("Gráfico",plotOutput("ana_che")),
                          #tabPanel("Tempo de atendimento",plotOutput("temp_at")),
                          #tabPanel("Tempo de espera",plotOutput("temp_che")),
                          tabPanel("Vizualizando", sidebarLayout(
                              sidebarPanel(
                               numericInput(inputId = "num",
                                           label = "Número  de atendentes",
                                          value = 1)
                            
                            ),
                             mainPanel(plotOutput(outputId = "hist"))
                            )),
                          tabPanel("Tempo de espera", sidebarLayout(
                            sidebarPanel(
                              numericInput(inputId = "num_temp",
                                           label = "Número  de atendentes",
                                           value = 1)
                              
                            ),
                            mainPanel(plotOutput(outputId = "tempo"))
                          ))
                          )
              )
              ),
              tabPanel("Filas",tabsetPanel(id="Gráfico",plotOutput("ate")))
   )
   
   
   server <- function(input, output) {
     
     chegadas <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-chegada.csv")
     tempo_atendimento <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-tempo_atendimento.csv")
     #df <- data.frame(chegadas)
     
     
     #output$hist <- renderPlot({hist(rnorm(input$num))})
     
     output$ana_che<-renderPlot({
       ggplot(data.frame(chegadas$min), aes(x=chegadas$min, y=chegadas$nc))+
         geom_point(aes(x=chegadas$min,y=chegadas$nc))
     })
     
     output$tempo<-renderPlot({
       lambda <- mean(chegadas$nc)
       
       mu_chegadas <- 1/lambda
       
       
       tea <- mean(tempo_atendimento$x)
       
       mu_atendimentos <- input$num_temp/tea

       N <-  1000
       
       t_max <- 60*(16-11 )
       
       set.seed(28052019)
       
       tempos_chegadas <- rexp( N ,rate = mu_chegadas)
       
       tempos_chegadas_acum <- cumsum(tempos_chegadas)
       
       tempos_chegadas_acum <- tempos_chegadas_acum[tempos_chegadas_acum<=t_max]
       
       n_clientes_entrada <- length(tempos_chegadas_acum)
       
       tempos_atendimentos <- rexp(n = n_clientes_entrada,rate = mu_atendimentos)
       
       tempos_atendimentos_acum <- cumsum(tempos_atendimentos)
       
       rho <- lambda/( 1 * mu_atendimentos )
       
       
      
       tempo_espera <- tempos_atendimentos_acum - tempos_chegadas_acum
       
       
       plot(1:n_clientes_entrada,
            tempo_espera,
            type ="l",
            xlab = "Cliente",
            ylab = "Tempo de espera ",
            main = "Tempo de espera por cliente (10h Ã s 16h)")
     })
     
     
     output$hist<-renderPlot({
       
       
       ## Ritmo de chegada ----
       
       lambda <- mean(chegadas$nc)
       
       ## Tempo mÃ©dio entre chegadas
       mu_chegadas <- 1/lambda
       
       
       ## Ritmo mÃ©dio de atendimento
       tea <- mean(tempo_atendimento$x)
       
       mu_atendimentos <- input$num/tea
       
       
       
       ### SimulaÃ§Ã£o 1 (bÃ¡sica) ----
       N <-  1000
       
       # instante no tempo em 16h
       t_max <- 60*(16-11 )
       
       # semente aleatÃ³ria
       set.seed(28052019)
       
       # simulando tempo entre chegadas
       tempos_chegadas <- rexp( N ,rate = mu_chegadas)
       
       # verificando instantes no tempo
       tempos_chegadas_acum <- cumsum(tempos_chegadas)
       
       # Filtrando clientes que chegaram depois das 16h
       tempos_chegadas_acum <- tempos_chegadas_acum[tempos_chegadas_acum<=t_max]
       
       # nÃºmero de clientes entre 11h e 16h
       n_clientes_entrada <- length(tempos_chegadas_acum)
       
       # tempo de atendimento de cada cliente
       tempos_atendimentos <- rexp(n = n_clientes_entrada,rate = mu_atendimentos)
       
       
       # tempo de atendimento de cada cliente (considerando 1 atendente)
       tempos_atendimentos_acum <- cumsum(tempos_atendimentos)
       
       
       ### Taxa de utilizaÃ§Ã£o dos atendentes ----
       
       rho <- lambda/( 1 * mu_atendimentos )
       
       # Calculando tempo de espera
       tempo_espera <- tempos_atendimentos_acum - tempos_chegadas_acum
       plot(tempos_atendimentos_acum/60 + 11,tempos_chegadas_acum/60 + 11,
            type ="l",
            xlab = "Tempo de atendimento dos atendentes",
            ylab = "Tempo de chegada ",
            main = "Instantes no tempo (10h as 16h)")
     })
     
    
     
     
     
     
   }
   
   shinyApp(ui = ui, server = server)
