## DDP week 4 ##

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)

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

cases <- as.data.frame.matrix(xtabs(covid_week_eu$cases ~ covid_week_eu$week + covid_week_eu$countries))
deaths <- as.data.frame.matrix(xtabs(covid_week_eu$deaths ~ covid_week_eu$week + covid_week_eu$countries))

#plotting
plot(cases$Netherlands, type = "l", 
     ylab = "Cases",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 cases per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(cases$Netherlands), max(cases$Spain)))),
     col = "blue")
lines(cases$Spain, col = "red")

plot(deaths$Netherlands, type = "l", 
     ylab = "Deaths",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 deaths per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(deaths$Netherlands), max(deaths$Spain)))),
     col = "blue")
lines(deaths$Spain, col = "red")









#subsetting
line1 <- covid_week_eu[covid_week_eu$countries == "Albania",]
line2 <- covid_week_eu[covid_week_eu$countries == "Italy",]

#plotting
plot(line1$week, line1$cases, type = "line", 
     ylab = "Cases",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 cases per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(line1$cases), max(line2$cases)))),
     col = "blue")
lines(line2$week, line2$cases, col = "red")

plot(line1$week, line1$deaths, type = "line", 
     ylab = "Deaths",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 deaths per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(line1$deaths), max(line2$deaths)))),
     col = "blue")
lines(line2$week, line2$deaths, col = "red")

plot(covid_week_eu$week[covid_week_eu$countries == "Netherlands"], 
     covid_week_eu$cases[covid_week_eu$countries == "Netherlands"], type = "l", 
     ylab = "Cases",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 cases per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(covid_week_eu$cases[covid_week_eu$countries == "Netherlands"]), 
                       max(covid_week_eu$cases[covid_week_eu$countries == "Portugal"])))),
     col = "blue")
lines(covid_week_eu$week[covid_week_eu$countries == "Portugal"], 
      covid_week_eu$cases[covid_week_eu$countries == "Portugal"], col = "red")

plot(covid_week_eu$week[covid_week_eu$countries == "Netherlands"], 
     covid_week_eu$deaths[covid_week_eu$countries == "Netherlands"], type = "l", 
     ylab = "Deaths",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 deaths per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(covid_week_eu$deaths[covid_week_eu$countries == "Netherlands"]), 
                       max(covid_week_eu$deaths[covid_week_eu$countries == "Portugal"])))),
     col = "blue")
lines(covid_week_eu$week[covid_week_eu$countries == "Portugal"], 
      covid_week_eu$deaths[covid_week_eu$countries == "Portugal"], col = "red")


