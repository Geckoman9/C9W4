#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$ui <- renderUI({
    if(is.null(input$Norm_Spec))
      return()
    switch(input$Norm_Spec,
    "Normal" = radioButtons("pieces", label = h3("Chess Pieces"),
                 choices = list("King" = 1, "Queen" = 2,
                                "Rook" = 3, "Knight" = 4,
                                "Bishop" = 5, "Pawn" = 6)),
    "Special" = radioButtons("special", label = h3("Special Moves/Takes"),
                 choices = list("En Passant" = 1, "King Side Castle" = 2,
                                "Queen Side Castle" = 3, "Promotion" = 4))
  )})
    
  output$move <- renderImage({
    if(input$Norm_Spec == "Normal" && input$pieces == 1){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "King_moves.png"))
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 2){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Queen_moves.png"))
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 3){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Rook_moves.png"))
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 4){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Knight_moves.png"))
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 5){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Bishop_moves.png"))
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 6){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Pawn_moves.png"))
    }
    else if(input$Norm_Spec == "Special" && input$special == 1){
        filename <- normalizePath(file.path("~/ChessDisplay/www", "Enpassant.png"))
    }
    else if(input$Norm_Spec == "Special" &&  input$special == 2){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "King_castle.png"))
    }
    else if(input$Norm_Spec == "Special" &&  input$special == 3){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Queen_castle.png"))
    }
    else if(input$Norm_Spec == "Special" &&   input$special == 4){
      filename <- normalizePath(file.path("~/ChessDisplay/www", "Promotion.png"))
    }
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$info <- renderText({
    if(input$Norm_Spec == "Normal" && input$pieces == 1){
      info <- "The King can move/take other pieces in any direction
      within one unoccupied tile of himself."
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 2){
      info <- "The Queen can move/take in any vertical, horizontal, or diagonal 
      direction as far as is not interrupted by another piece."
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 3){
      info <- "The Rook can move/take in any vertical or horizontal direction
      as far as is not interrupted by another piece."
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 4){
      info <- "The Knight can move/take in a special L movement. Two squares
      away and then one square perpendicular to that movement."
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 5){
      info <- "The Bishop can move/take in any diagonal direction as far as
      is not interrupted by another piece. The Bishop never changes square color."
    }
    else if(input$Norm_Spec == "Normal" && input$pieces == 6){
      info <- "The pawn traditionally moves forward one space. However, on its
      first move, it is allowed to move two spaces. Also, the pawn takes
      diagonally one space."
    }
    else if(input$Norm_Spec == "Special" && input$special == 1){
      info <- "When a pawn goes two spaces for its first move, it opens itself to be
      taken via en passant (translated from French, 'in passing'). If the pawn in
      question goes two spaces and ends up next to an enemy's pawn, the enemy pawn's
      next turn can be used to take that pawn as if the pawn in question had only
      moved one space. This is a rare and usually unhelpful special move."
    }
    else if(input$Norm_Spec == "Special" && input$special == 2){
      info <- "The king can move two squares towards his rook and the rook can 
      jump to the square directly on the opposite side of the king. This can
      only happen as long as neither the king nor rook has moved and the king is
      not in check or passing through check."
    }
    else if(input$Norm_Spec == "Special" && input$special == 3){
      info <- "The king can move two squares towards the queen's rook and the rook
      can jump to the square directly on the opposite side of the king. This can
      only happen as long as neither the king nor rook has moved and the king is
      not in check or passing through check."
    }
    else if(input$Norm_Spec == "Special" && input$special == 4){
      info <- "As a pawn arrives to the opposite side of the board from where he
      started, he has the option to promote into one of the other four pieces on the
      board (exluding the king)."
    }
  })
  
})
