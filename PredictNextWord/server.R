#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("PredictNextWordForApp.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observeEvent(input$inputText,
        output$predictions <- renderTable(predictNextWord(input$inputText,
                                 noWordsToReturn = input$noWords,
                                 skipPenalty = 2,
                                 removeStopWords=FALSE,
                                 removeWordSuffixes=FALSE)[,c("word"),
                                                           drop=FALSE],
            hover=TRUE,
            striped=TRUE,
            bordered=TRUE,
            rownames=TRUE,
            colnames=TRUE)
    )
})
