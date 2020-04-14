##### Packages #####
library(ggplot2) # required for 2 dataframe scatterplot
library(ggvis)

# required for world map
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

##### Define Server #####
server <- function(input, output){

  ##### WORLD MAP COMPONENT #####
    
    #### -------------------------------- SET UP THE CODE THAT ONLY NEEDS TO BE RUN ONCE ------------------------------------------------ ####
    
    # Append ISO3 codes to the map.word data to be able to merge long/lat polygon data with the other data frames 
    map.world<-map_data("world")
    map.world$ISO = 0
    map.world$ISO<-countrycode(map.world$region,"country.name","iso3c")
    head(map.world)
    
    # Create dataframes from left joining GDP,Literacy and Mortality to map.world
    map.world_joined<-left_join(map.world,gdpFinal,by=c('ISO'='ISO3'))                   # GDP 
    map.world_joined2<-left_join(map.world,literacyMaleFinal,by=c('ISO'='ISO3'))         # Male Literacy 
    map.world_joined3<-left_join(map.world,literacyFemaleFinal,by=c('ISO'='ISO3'))       # Female Literacy 
    map.world_joined4<-left_join(map.world,mortalityMaleFinal,by=c('ISO'='ISO3'))        # Male Mortality
    map.world_joined5<-left_join(map.world,mortalityFemaleFinal,by=c('ISO'='ISO3'))      # Female Mortality
    
    #### ------------------------------ CREATE A MAP AFTER CHECKING FOR UPDATED MAP DATA ----------------------------------------- #####
    
    # Get a stationary non iteractive plot to show on the GUI
    output$show_plot <- renderPlotly({
      
      Start_year <- as.numeric(input$year_map)
      name <- input$map_factor
      
      # Get DataSource, Contributor and map subtitle from the Name of Plot
      if(name=="Female Infant Mortality Rate per 1000, under 5"){
        DataSource = map.world_joined5
        Contributor = "World Health Organization"
        data_units = "number of female children under 5 who died per 1000 live births"
        legend = "mortality rate"
      }else{
        if(name=="Male Infant Mortality Rate per 1000, under 5"){
          DataSource = map.world_joined4
          Contributor = "World Health Organization"
          data_units = "number of male children under 5 who died per 1000 live births"
          legend = "mortality rate"
        }else{
          if(name=="Real GDP per Capita, 2010 US Dollars"){
            DataSource = map.world_joined
            Contributor = "World Bank"
            data_units = "GDP in US dollars per capita"
            legend = "GDP per capita"
          }else{
            if(name=="Female Literacy Rate, over 15 years old"){
              DataSource = map.world_joined3
              Contributor = "World Bank"
              data_units = "percent of females who are literate"
              legend = "% female literacy"
            }else{
              if(name=="Male Literacy Rate, over 15 years old"){
                DataSource = map.world_joined2
                Contributor = "World Bank"
                data_units = "percent of males who are literate"
                legend = "% male literacy"
              }
            }
          }
        }
      }
      
      # Get column corresponding to user specified date and create a boolean column for missing values
      a = as.numeric(which(names(DataSource)==Start_year))                        # get the column corresponding to user specified date 
      
      # Apply a logarithmic scale to normalize the color distribution of the world map for GDP
      Selected = DataSource[,a]                                                             # Data for choosen attribute for user selected year
      DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
      DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))         # Apply ln transform to normalize colors on graph
      
      # CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale) based on 
      # type of data set and "spread" of the data altogether
      
      if(name=="Real GDP per Capita, 2010 US Dollars"){
        filler = DataSource$lnfactors
      }else{
        filler = DataSource$factors}
      
      # See what Type of plot you are using -- (Could be replaced by just a name check but who knows if you'll need it later)
      ln = na.omit(ifelse(filler==DataSource$factors,"linear","logarithmic"))
      plotType = ln[1]
      
      # Calculate the scale to customize the legend used in the Map
      max_n = max(na.omit(filler))
      min_n = min(na.omit(filler))
      range_n = max_n-min_n
      size_ticks = range_n/6
      
      
      # Get the units of the tick marks on the legend set
      tick0 = min_n                                                
      tick1 = tick0 + size_ticks
      tick2 = tick1 + size_ticks
      tick3 = tick2 + size_ticks
      tick4 = tick3 + size_ticks
      tick5 = tick4 + size_ticks
      tick6 = max_n
      
      # Get quartiles for the legend in linear and log format
      if(plotType=="linear"){
        max_value = as.integer(max(na.omit(DataSource[,a])))           
        min_value = as.integer(min(na.omit(DataSource[,a])))
        
        q1_value = as.integer((1*(max_value-min_value)/6)+min_value)
        q2_value = as.integer((2*(max_value-min_value)/6)+min_value)
        q3_value = as.integer((3*(max_value-min_value)/6)+min_value)
        q4_value = as.integer((4*(max_value-min_value)/6)+min_value)
        q5_value = as.integer((5*(max_value-min_value)/6)+min_value)
      }
      
      if(plotType=="logarithmic"){
        max_value = as.integer(max(na.omit(DataSource[,a]))) 
        max_ln = log(max_value)
        min_value = as.integer(min(na.omit(DataSource[,a])))
        min_ln = log(min_value)
        diff = max_ln - min_ln
        
        q1_value_ln = ((1*(diff)/6)+min_ln)
        q2_value_ln = ((2*(diff)/6)+min_ln)
        q3_value_ln = ((3*(diff)/6)+min_ln)
        q4_value_ln = ((4*(diff)/6)+min_ln)
        q5_value_ln = ((5*(diff)/6)+min_ln)
        
        q1_value = round(exp(q1_value_ln),-2)
        q2_value = round(exp(q2_value_ln),-2)
        q3_value = round(exp(q3_value_ln),-3)
        q4_value = round(exp(q4_value_ln),-3)
        q5_value = round(exp(q5_value_ln),-3)
        
        max_value = round(max_value,-3)
        min_value = round(min_value,-2)
      }
      
      #Set labels for the legend ticks
      lab0 = min_value 
      lab1 = q1_value
      lab2 = q2_value
      lab3 = q3_value
      lab4 = q4_value
      lab5 = q5_value
      lab6 = max_value
      
      myMap<-ggplot() +
        geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler, text = paste("Country: ", region, "<br>", legend, as.integer(DataSource[,a])))) +
        labs(title = paste(Start_year,name,sep = " "),subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."),caption = paste("source: ",Contributor)) +
        scale_fill_gradientn(name=legend,colours = brewer.pal(5, "RdYlBu"), na.value = 'white',
                             breaks=c(tick0,tick1,tick2,tick3,tick4,tick5,tick6),
                             labels=c(lab0,lab1,lab2,lab3,lab4,lab5,lab6)) +
        theme(text = element_text(color = "#FFFFFF")
              ,panel.background = element_rect(fill = "#444444")
              ,plot.background = element_rect(fill = "#444444")
              ,panel.grid = element_blank()
              ,plot.title = element_text(size = 20)
              ,plot.subtitle = element_text(size = 10)
              ,axis.text = element_blank()
              ,axis.title = element_blank()
              ,axis.ticks = element_blank()
              ,legend.position = "right"
              ,legend.background = element_rect(fill = "#444444")
        )
      
      # generate the plot output
      ggplotly(myMap,tooltip = "text")
    })

    #### ------------------------------------ CREATE THE CODE TO PRODUCE RESULTS AFTER HOVERING OVER MAP WITH MOUSE ----------------------- ####
    
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      point <- nearPoints(DataSource, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property for tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Country: </b>", point$region, "<br/>",
                      "<b> Value: </b>", as.integer(point[,a]), "<br/>")))
        
      )
    })
  
  ##### SCATTERPLOT COMPONENT #####
  # 
  # scatterPlotXData <- reactive({
  #   if ("Real GDP per Capita, 2010 US Dollars" %in% input$x_factor) return(gdpFinal)
  #   if ("Male Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyMaleFinal)
  #   if ("Female Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyFemaleFinal)
  #   if ("Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityBTSXFinal)
  #   if ("Female Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortailityFemaleFinal)
  #   if ("Male Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityMaleFinal)
  # })
  # 
  # scatterPlotYData <- reactive({
  #   if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return(gdpFinal)
  #   if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyMaleFinal)
  #   if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyFemaleFinal)
  #   if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityBTSXFinal)
  #   if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortailityFemaleFinal)
  #   if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityMaleFinal)
  # })
  # 
  # 
  # # A reactive expression with the ggvis plot
  # scatterDataMerge <- reactive({
  #   x_data <- as.data.frame(scatterPlotXData()) %>% select(c("ISO2", toString(input$year_scatter))) # selects correct year and all countries
  #   y_data <- as.data.frame(scatterPlotYData()) %>% select(c("ISO2", toString(input$year_scatter))) # selects correct year and all countries
  #   return(left_join(x_data, y_data, by = c("ISO2" = "ISO2")))
  # })
  
# output$testPlot <- renderPlot({ggplot(data = scatterDataMerge())+geom_point(x=input$year, y=input$year)})

  
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

#shinyApp(ui = ui, server = server)
