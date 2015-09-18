
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  
  x <- reactive({input$id1 * 10})
  
  output$op3 <- renderPrint({
    
    input$btn1
    isolate({input$dateID1})
    
  })
  output$op2 <- renderPrint({input$chkboxID1})
  output$op1 <- renderPrint({x()})
  output$op4 <- renderText({input$dateID1})
  
  output$op5 <- renderText({
    
    if(input$btn1 == 0 ) "please press the button"
    else if (input$btn1 == 1) "You pressed it once"
    else paste("Ok you pressed it lot many times" , input$btn1)
  })
  
})
