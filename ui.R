library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Next Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter some text from which to predict the next word."),
    
      textInput("usergram", "", "Enter some text in my"),
      actionButton("getWordsbtn", "Predict")
      
    ),
      mainPanel(
          h4("The top five word predictions are;"),
          textOutput("text1")
      )
    )
  ))
