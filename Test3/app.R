#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#TEST 3

library(shiny)
library(plotly)
library(ggplot2)

# Collecting data
covid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_eu <- covid %>%
        filter(continentExp == "Europe") %>%
        mutate(dateChar = dateRep) %>%
        mutate(dateRep = as.Date(as.character(dateRep), format = "%d/%m/%Y")) #%>%

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
                     value = 30)
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
      x    <- faithful[,2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      # plot date vs. cases
      plot <- plot_ly(data = covid_eu, 
                      x = covid_eu$dateRep, 
                      y = covid_eu$cases, 
                      name = "Cases", 
                      type = "scatter", 
                      mode = "lines")
      plot <- plot %>% add_trace(y = covid_eu$deaths,
                                 name = "Deaths",
                                 mode = "lines",
                                 line = list(color = "rgb(200,0,0)"))
      plot <- plot %>% layout(title = "Confirmed COVID-19 cases and deaths in the Netherlands over time",
                              xaxis = list(title = "Date"),
                              yaxis = list (title = "Cases and deaths"))
      plot 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

