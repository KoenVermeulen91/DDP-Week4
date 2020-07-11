#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$plot1 <- renderPlot({
                plot(1:10, 1:10)
        })
        
        output$plot2 <- renderPlot({
                plot(1:10 ,10:1)
        })
        
        output$dis <- renderDataTable({})
})
