#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        # generate bins based on input$bins from ui.R
        line1 <- covid_week_eu[covid_week_eu$countries == "Albania",]
        line2 <- covid_week_eu[covid_week_eu$countries == "Italy",]
        
        # plot 1: Cases per week
        output$distPlot <- renderPlot({
                plot(line1$week, line1$cases, type = "line", 
                     ylab = "Cases",
                     xlab = "Weeknumber",
                     main = "Comparing countries on COVID-19 cases per week",
                     xlim = c(0, 30),
                     ylim = c(0, max(c(max(line1$cases), max(line2$cases)))),
                     col = "blue")
                lines(line2$week, line2$cases, col = "red")
  })
  
  # plot 2: deaths per week
  output$distPlot <- renderPlot({
          plot(line1$week, line1$deaths, type = "line", 
               ylab = "Deaths",
               xlab = "Weeknumber",
               main = "Comparing countries on COVID-19 deaths per week",
               xlim = c(0, 30),
               ylim = c(0, max(c(max(line1$deaths), max(line2$deaths)))),
               col = "blue")
          lines(line2$week, line2$deaths, col = "red")
  })
  
})
