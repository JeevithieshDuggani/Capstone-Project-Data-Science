library(shiny)

shinyUI(
    pageWithSidebar(
        headerPanel(
            h2("Write From the Last Word - Data Science Specialization Capstone Project")
        ),
        
    sidebarPanel(
        textInput(inputId = "inputText",label = "Sentence to be Completed"),
        actionButton("goButton", "Enter")
    ),
    mainPanel(
        p('The text Entered : '),
        verbatimTextOutput('oText'),
        p('The predicted next word'),
        verbatimTextOutput('nextWord'),
    )
))