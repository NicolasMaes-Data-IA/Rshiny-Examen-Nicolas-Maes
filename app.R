library(shiny)
library(dplyr)
library(ggplot2)
library(shinylive)
library(DT)
library(bslib)
library(thematic)

data("diamonds")

# GESTION UI
ui <- fluidPage(
      theme = bs_theme(version = 5, bootswatch = "minty"), 
      titlePanel("Exploration Diamonds"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "Couleurs",
                  label = "Choisir une couleur à filtrer :",
                  choices = c("D", "E", "F", "G", "H", "I", "J"),
                  selected = "D"),
      sliderInput(inputId = "prix",
                  label = "Prix maximum :",
                  min = 0,
                  max = 20000,
                  value = 5000),
    ), 
    
    
    mainPanel(
      plotOutput("distPlot")
    )
  ) 
) 

# GESTION SERVEUR
server <- function(input, output) {
    
    rv <- reactiveValues(df = diamonds, choix_rose = "Non")
    
    observeEvent(input$bouton,
                 rv$df <- diamonds |> 
                 filter(color == input$Couleurs) |> 
                 filter(price <= input$prix))

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
