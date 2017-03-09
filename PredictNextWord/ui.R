#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    # Application title
    titlePanel("Predict Next Word Demonstartion Application"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput(inputId="inputText",
                          label="Please enter text to use here",
                          value="",
                          cols=20,
                          rows=4,
                          placeholder = "hint: this is where you enter the text",
                          resize="vertical"),
           sliderInput("noWords",
                       "Number of Words to Predict",
                       min = 1,
                       max = 10,
                       value = 1),
           helpText("Enter some text above and select the number of words with the slider that you would like to predict.")  #,
           #actionButton("PredictButton", "Start")
        ),
    # Show a plot of the generated distribution
        mainPanel(
            h4("The Predictions Are:"),
            tableOutput("predictions"),
            renderText("inputText")
        )
    )
))
