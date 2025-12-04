# Version finale

library(shiny)
library(dplyr)
library(ggplot2)
library(shinylive)
library(DT)
library(bslib)
library(thematic)
library(plotly)

data("diamonds")

# GESTION UI
ui <- fluidPage(
      theme = bs_theme(version = 5, bootswatch = "minty"), 
      titlePanel("Exploration Diamonds"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "rose",
                   label = "Colorier en rose ?",
                   choices = c("Oui", "Non"),
                   selected = "Non"),
      
      selectInput(inputId = "Couleurs",
                  label = "Choisir une couleur à filtrer :",
                  choices = levels(diamonds$color),
                  selected = "D"),
      
      sliderInput(inputId = "prix",
                  label = "Prix maximum :",
                  min = 0,
                  max = 20000,
                  value = 5000),
      
      actionButton(inputId = "bouton", 
                   label = "Visualiser le graph")
    ), 
    
    
    mainPanel(
      plotlyOutput("distPlot"),
      DTOutput("monTableau")
    )
  ) 
) 

# GESTION SERVEUR
server <- function(input, output) {
    
    rv <- reactiveValues(df = diamonds, choix_rose = "Non")
    
    observeEvent(input$bouton, {
      rv$df <- diamonds |> 
        filter(color == input$Couleurs) |> 
        filter(price <= input$prix)
      rv$choix_rose <- input$rose
      
    })
                 
    output$distPlot <- renderPlotly({
      couleur_point <- ifelse(rv$choix_rose == "Oui", "pink", "black")
      g <- ggplot(rv$df, aes(x = carat, y = price)) +
        geom_point(color = couleur_point) +
        theme_minimal() +
        labs(title = paste("Diamants couleur", input$Couleurs))
      ggplotly(g)
    })
    
    output$monTableau <- renderDT({
      rv$df
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
