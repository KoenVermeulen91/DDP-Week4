#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
              tabsetPanel(
                      tabPanel(plotOutput("distPlot")),
                      tabPanel(plotOutput("distPlot")),
                      tabPanel(plotOutput("distPlot"))
                      
              )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        # Collecting data
        covid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
        
        # Data prep Europe per week
        covid_eu <- covid %>%
                filter(continentExp == "Europe") %>%
                filter(year == 2020) %>%
                mutate(dateChar = dateRep) %>%
                mutate(dateRep = as.Date(as.character(dateRep), format = "%d/%m/%Y")) %>%
                mutate(cases = ifelse(cases < 0, 0, cases)) %>%
                mutate(deaths = ifelse(deaths < 0, 0, deaths)) %>%
                mutate(weekday = wday(dateRep)) %>%
                mutate(week = as.integer(week(dateRep))) %>%
                mutate(countries = as.character(countriesAndTerritories))
        
        covid_week_eu <- covid_eu %>%
                group_by(week, countries) %>%
                summarise(cases = sum(cases),
                          deaths = sum(deaths))
        
        countries <- unique(covid_week_eu$countries, fromLast = T)
   
        
        
   output$distPlot <- renderPlot({
           bins <- seq(min(x), max(x), length.out = input$bins + 1)
           line1 <- covid_week_eu[covid_week_eu$countries == "Albania",]
           line2 <- covid_week_eu[covid_week_eu$countries == "Italy",]
           
           # draw the histogram with the specified number of bins
           hist(faithful$waiting, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

