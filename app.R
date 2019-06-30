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
   tabPanel("Dispersão",tabsetPanel(tabPanel("Chegadas x tempo atendimento",plotOutput("ana_che"))))
   )
   
   server <- function(input, output) {
     
     chegadas <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-chegada.csv")
     tempo_atendimento <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-tempo_atendimento.csv")
     #df <- data.frame(chegadas)
     
     #output$hist <- renderPlot({hist(rnorm(input$num))})
     
     output$ana_che<-renderPlot({
       ggplot(data.frame(chegadas$min), aes(x=a, y=b))+
         geom_point(aes(x=chegadas$min,y=chegadas$nc))
     })
     
     
   }
   
   shinyApp(ui = ui, server = server)
