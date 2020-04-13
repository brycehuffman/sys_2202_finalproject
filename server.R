library(ggplot2) # required for 2 dataframe scatterplot
server <- function(input, output){

  worldMapData <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return(gdpFinal)
    if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyMaleFinal)
    if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyFemaleFinal)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityBTSXFinal)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortailityFemaleFinal)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityMaleFinal)
  })
  
  
  scatterPlotXData <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$x_factor) return(gdpFinal)
    if ("Male Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyMaleFinal)
    if ("Female Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyFemaleFinal)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityBTSXFinal)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortailityFemaleFinal)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityMaleFinal)
  })
  
  scatterPlotYData <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return(gdpFinal)
    if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyMaleFinal)
    if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyFemaleFinal)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityBTSXFinal)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortailityFemaleFinal)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityMaleFinal)
  })
  
  
  
  
  
  # if (input$map_factor == "Real GDP per Capita, 2010 US Dollars"){
  #   mapData = gdpFinal
  # }
  # if (input$map_factor == "Male Literacy Rate, over 15 years old"){
  #   mapData = literacyMaleFinal
  # }
  # if (input$map_factor == "Female Literacy Rate, over 15 years old"){
  #   mapData = literacyFemaleFinal
  # }
  # if (input$map_factor == "Infant Mortality Rate per 1000, under 5"){
  #   mapData = mortalityBTSXFinal
  # }
  # if (input$map_factor == "Female Infant Mortality Rate per 1000, under 5"){
  #   mapData = mortalityFemaleFinal
  # }
  # if (input$map_factor == "Male Infant Mortality Rate per 1000, under 5"){
  #   mapData = mortalityFemaleFinal
  # }
  # output$testPlot <- renderPlot({ggplot()
  
  # output$testPlot <- renderPlot({ggplot()
  #   hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    
    
    
    
  
  
}