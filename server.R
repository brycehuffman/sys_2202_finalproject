##### Packages #####
library(ggplot2) # required for 2 dataframe scatterplot
library(ggvis)

##### Define Server #####
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
  
  
  # A reactive expression with the ggvis plot
  scatterDataMerge <- reactive({
    x_data <- as.data.frame(scatterPlotXData()) %>% select(c("ISO2", toString(input$year_scatter))) # selects correct year and all countries
    y_data <- as.data.frame(scatterPlotYData()) %>% select(c("ISO2", toString(input$year_scatter))) # selects correct year and all countries
    return(left_join(x_data, y_data, by = c("ISO2" = "ISO2")))
  })
  
output$testPlot <- renderPlot({ggplot(data = scatterDataMerge())+geom_point(x=input$year, y=input$year)})

  
   #   ggvis(x = xvar, y = yvar) %>%
    #   layer_points(size := 50, size.hover := 200,
    #                fillOpacity := 0.2, fillOpacity.hover := 0.5,
    #                stroke = ~has_oscar, key := ~ID) %>%
    #   add_tooltip(movie_tooltip, "hover") %>%
    #   add_axis("x", title = input$x_factor) %>%
    #   add_axis("y", title = input$y_factor) %>%
    #   # add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
    #   # scale_nominal("stroke", domain = c("Yes", "No"),
    #   #               range = c("orange", "#aaa")) %>%
    #   set_options(width = 500, height = 500)

  
  # vis %>% bind_shiny("scatterPlot")
  
  # output$testPlot <- renderPlot({ggplot() +
  #     ggplot2::geom_point(data=scatterPlotXData, aes(input$year)) +
  #     ggplot2::geom_point(data=scatterPlotYData, aes(input$year))})

  

    
  
} # close server