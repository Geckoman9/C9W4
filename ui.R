#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define a UI that shows the different moves of a chess piece.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("How do chess pieces move?"),
  
  # Sidebar with radio buttons to pick a piece. 
  sidebarLayout(
    sidebarPanel(
      selectInput("Norm_Spec", "Types of Move", c("Normal","Special")),
      uiOutput("ui")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("info"),
       imageOutput("move")
    )
  )
))
