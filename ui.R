# R Shiny UI
library(shiny)

# define the UI
server <- function(input, output){
  
}

ui <- fluidPage(
  titlePanel("Health and Economic Factors Visualization"),
  fluidRow(
    column(6,
      wellPanel(
        h4("Select World Map Options"),
          selectInput("map_factor", "Choose a factor for world map visualization.",
            c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
              "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
              "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5")
          )
        ),
      wellPanel(
        h4("Select Scatterplot Options"),
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
          "<><><><><><><this is a place to add notes><><><><><><>")
                    )
      )

    )
  )
  
  
)

shinyApp(ui=ui, server=server)