
library(shiny)
library(markdown)

## SHINY UI
shinyUI(
  fluidPage(
    titlePanel("Data Science Capstone - Word prediction"),
    sidebarLayout(
      sidebarPanel(
        helpText("ENTER TEXT"),
        hr(),
        textInput("inputText", "ENTER THE TEXT / WORD / SENTENCE HERE",value = ""),
        checkboxInput("avoidProfanity","Avoid profanity words", value = TRUE),
        hr(),
        helpText("A box with the most probable following words is shown as first result", 
                 hr(),
                 "Then, the original text with the next predicted word is displayed.",
                 hr(),
                 "The code has to prepare the n-grams, so first time you use it can be a bit more slow"),
        hr(),
        hr()
      ),
      mainPanel(
        hr(),
        h3("WARNING: It could be slow with the first sentence"),
        "First time you use it it needs to load packages and read some info. It shoud be fast afterwards.",
        h2("Next predicted words"),
        verbatimTextOutput("prediction"),
        br(),
        hr(),
        strong("Predicted sentece"),
        strong(code(textOutput('predSentence')))
      )
    )
  )
)