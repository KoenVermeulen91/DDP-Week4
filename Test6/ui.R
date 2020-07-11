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
shinyApp(
        shinyUI(
                fluidPage(
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Summary", dataTableOutput("dis")),
                                        tabPanel("Plot",
                                                 # fluidRow(...)
                                                 plotOutput("plot1"),
                                                 plotOutput("plot2")
                                        )
                                )
                        )
                )
        )
)