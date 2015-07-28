library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Car Price Estimator V0.1 Charles Copley"),
  sidebarPanel(
    textInput(inputId="text1", label = "Car Make e.g. Toyota, Audi, Chevrolet",value='Toyota'),
    textInput(inputId="text2", label = "Model e.g. Corolla, A4, Spark",'Corolla'),
    textInput(inputId="text3", label = "Year e.g. 2009",value='2009'),
    textInput(inputId="text4", label = "Engine e.g. 1.8",value=1.8),
    textInput(inputId="text5", label = "Mileage (in km) e.g. 120000",value='120000'),
    actionButton("goButton","Go!")      
  ),
  mainPanel(
  #  p('Output Text1'),
  #  textOutput('text1'),
  #  p('Output Text2'),    
  #  textOutput('text2'),
    p('Price of Car'),
    textOutput('text3'),
    plotOutput("depreciation")
    
  )
  )
  )


# shinyUI(pageWithSidebar(
#   headerPanel("Hello Shiny!"),
#   sidebarPanel(
#     textInput(inputId="text1", label = "Input Text1"),
#     textInput(inputId="text2", label = "Input Text2"),
#     actionButton("goButton", "Go!")
#   ),
#   mainPanel(
#     p('Output text1'),
#     textOutput('text1'),
#     p('Output text2'),
#     textOutput('text2'),
#     p('Output text3'),
#     textOutput('text3')
#   )
# ))

