#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
        titlePanel(title = h3("COVID-19: Comparing European countries", align="center")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
              sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = c(20, 30)
                     ),
              
              sliderInput("DateRange", "Select date range: ", 
                          min = as.Date("01-01-2020", format = "%d/%m/%Y"), 
                          max = today(), 
                          value = c(as.Date("01-01-2020", format = "%d/%m/%Y"), today()), step=1
                          )
      ),
      
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(covid_eu, breaks = bins, col = 'darkgray', border = 'white')
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

