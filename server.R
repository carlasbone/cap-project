# server.R
library(shiny)
library(shinyapps)
library(data.table)
library(stringr)
library(SnowballC)
source("helpers.R")

    shinyServer(function(input, output) {
        
        dataInput2 <- eventReactive(input$getWordsbtn, {
            userwords <- trimstring(input$usergram)
            userwords <-  unlist(str_split(userwords," "))
            
            getnextwords(userwords)
        })
          output$text1 <-  renderText({
              paste0(dataInput2(),collapse=", ")
           })
        
    })
    