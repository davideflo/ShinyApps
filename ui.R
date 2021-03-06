###### ui.R file for shiny app

library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinyjs)


# Define UI for Quoting
  #pageWithSidebar(
  fluidPage(
    
    useShinyjs(),
    
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Quoting"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  
  
  sidebarPanel(
    selectInput("select", label = h3("Seleziona tipo di quoting"), 
                choices = list("quoting completo" = 1, "quoting Tecla" = 2)),
    actionButton("Action", label = "Esegui Quoting"),
    h6("(ci mette circa 3 minuti)")
  ),
  
  sidebarPanel(
    textOutput("oldpun7"),
    textOutput("oldpun8")
  ),
  
  sidebarPanel(
    textOutput("newpun7"),
    textOutput("newpun8"),
    textOutput("time")
  ),
  
  
  mainPanel(
    plotOutput("plot17"),
    plotOutput("plot18"),
    textOutput("mess7"),
    textOutput("mess8"),
    textOutput("quotingText")
  )
  
 
)

