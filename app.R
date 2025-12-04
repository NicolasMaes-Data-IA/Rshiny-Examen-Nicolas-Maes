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
                  label = "Choisir la couleur :",
                  choices = c("D", "E", "F", "G", "H", "I", "J"),
                  selected = "D")
    ), 
    
    
    mainPanel(
      plotOutput("distPlot")
    )
  ) 
) 

# GESTION SERVEUR
server <- function(input, output) {
  
    observeEvent(input$bouton)

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
