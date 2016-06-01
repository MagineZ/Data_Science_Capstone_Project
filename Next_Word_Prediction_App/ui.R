library(shiny)
shinyUI(
    navbarPage("Natural Language Prediction - Next Word Prediction", inverse = FALSE, collapsible = FALSE, 
                   tabPanel("Prediction", (includeScript("Google_Analytics.js")),
                            fluidRow(
                                sidebarPanel(width=3,
                                             h5("Text Input:"),
                                             textInput("entry", 
                                                       "The app will predict your next possible word you want to type. Now, please type your input here:",
                                                       "Nice to meet you"),
                                             # submitButton('Predict'),
                                             sliderInput("max", 
                                                         h5("Maximum Number of Words:"), 
                                                         min = 10,  max = 200,  value = 100),
                                             br(),
                                             actionButton("update", "Update Word Cloud"),
                                             hr(),
                                    helpText(h5("Help Instruction:")),
                                    helpText("1. Type your sentence in the text field", style="color:#428ee8"),
                                    helpText("2. The value will be passed to the model while you are typing.", style="color:#428ee8"),
                                    helpText("3. Obtain the instant predictions below.", style="color:#428ee8"),
                                    hr(),
                                    h6("For more information about Pei-Chun Su:"),
                                    a(img(src = "GitHub-Mark.png", height = 30, width = 30),href="https://github.com/MagineZ"),
                                    a(img(src = "gmail.jpeg", height = 30, width = 30),href="mailto: b94401079@gmail.com"),
                                    br()
                                ),
                                mainPanel(
                                    column(5,
                                    h3("Word Prediction"),hr(),
                                    h5('The sentence you just typed:'),                             
                                    wellPanel(span(h4(textOutput('sent')),style = "color:#428ee8")),
                                    hr(),
                                    h5('Single Word Prediction:'),
                                    wellPanel(span(h4(textOutput('top1')),style = "color:#e86042")),
                                    hr(),
                                    h5('Other Possible Single Word Predictions:'),
                                    wellPanel(span(h5(textOutput('top2')),style = "color:#2b8c1b"),
                                    span(h5(textOutput('top3')),style = "color:#2b8c1b"),
                                    span(h5(textOutput('top4')),style = "color:#2b8c1b"),
                                    span(h5(textOutput('top5')),style = "color:#2b8c1b")),
                                    hr(),
                                    
                                    p('Details of the prediction algorithm and source codes', 
                                      code("server.R"), code("ui.R"), code("Predict_func.R"), code("Tokenization_func.R"), code("ngramify_func.R"), 
                                      'can be found at', a("SwiftKey-Natural-language.",href="https://github.com/MagineZ"))
                                    ),
                                    column(5,
                                           h3("Word Cloud Diagram"),hr(),
                                           h5("Please click", code("Update Word Cloud"), 
                                              "button and", code("Slide Input"), "in the side bar to update the plot for relevant prediction."),
                                           plotOutput("wordCloud"), # wordcloud
                                           br()
                                           )
                                )
                            )
                   )
               )
    )
               
                   