
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Illustrating markup"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    h1("Sidebar Text h1"),
    h2("Sidebar Text h2"),
    h3("Sidebar Text h3"),
    h4("Sidebar Text h4 modified"),
    numericInput("id1","numeric input for ID1", value = 1, min=1, max=10, step=2),
    
    checkboxGroupInput("chkboxID1", "Checkboxinput for chkboxID1", c(
      "Value1Key"="1",
      "Value2Key"="2",
      "Value2Key"="3",
      "Value2Key"="4"
      
    )),
    dateInput("dateID1","Date:"),
   # submitButton("Apply Changes")
   actionButton("btn1", "Go Button")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    h3("Main Panel Text"),
    code("some code"),
    p('ordinary text'),
    
    verbatimTextOutput("op1"),
    verbatimTextOutput("op2"),
    textOutput("op3"),
    verbatimTextOutput("op4"),
    textOutput("op5")
    
  )
))
