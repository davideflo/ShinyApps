###### ui.R file for shiny app

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Quoting"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    actionButton("Action", label = "Esegui Quoting")
    # selectInput("Anno", "Anno:",
    #             list("2017" = "2017", 
    #                  "2018" = "2018"))
    
  ),
  mainPanel(
    #textOutput("pun7"),
    plotOutput("plot17")
  )
))