# R Shiny UI
library(shiny)
library(ggvis)
library(plotly)
library(ggmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(countrycode)
library(ggplot2)
library(ggiraph) 
library(RColorBrewer)

ui <- fluidPage(
  titlePanel("Health and Economic Factors Visualization"),
  fluidRow(
    column(4,
      wellPanel(
        h4("Select World Map Options"),
          selectInput("map_factor", "Choose a factor for world map visualization.",
            c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
              "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
              "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5")
          ),
          textInput("year_map", "Choose a year between 1950 and 2019:", value = "2018"),
        ),
      wellPanel(
        h4("Select Scatterplot Options"),
        textInput("year_scatter", "Choose a year between 1950 and 2019:", value = "2018"),
          selectInput("x_factor", "Choose a factor for the x-axis of the scatter plot",
            c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
              "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
              "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5")
          ),
        selectInput("y_factor", "Choose a factor for the y-axis of the scatter plot",
                    c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                      "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                      "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5")
        )
            
      ),
      wellPanel(
        h4("Notes"),
        tags$small(paste0(
          "<><><><><><><this is a place to add notes><><><><><><>"
          )
                    )
      ),
      column(8,
             wellPanel(
               #tableOutput('show_inputs') #Show the variable inputs to a table output in the Shiny App
             )
  

    )
      ),
    mainPanel(
        plotlyOutput('show_plot',width = 800,height = 500)
    ),
  )
)


shinyApp(ui = ui, server = server)

