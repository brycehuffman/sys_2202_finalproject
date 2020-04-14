##### LIBRARIES FROM UI #####

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")

##### LIBRARIES FOR SERVER: SCATTERPLOT

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggvis)) install.packages("ggvis", repos = "http://cran.us.r-project.org")


##### LIBRARIES FOR SERVER: WORLDMAP
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org") # interactive labels for plot
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") # interactive labels for plot


##### DATA PROCESSING (AS OF 4/13) #####

if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD", start = 1980, end = 2018)
write.csv(gdp, 'gdp.csv', row.names = FALSE)

literacyMale <- WDI(country = "all", indicator = "SE.ADT.LITR.MA.ZS", start = 1980, end = 2018)
write.csv(literacyMale, 'literacyMale.csv', row.names = FALSE)

literacyFemale <- WDI(country = "all", indicator = "SE.ADT.LITR.FE.ZS", start = 1980, end = 2018)
write.csv(literacyFemale, 'literacyFemale.csv', row.names = FALSE)

# Pull WHO data from API Query
mortalityUnder5 <- read.csv(url("https://apps.who.int/gho/athena/api/GHO/MDG_0000000007?format=csv"))
write.csv(mortalityUnder5, 'mortalityUnder5.csv', row.names = FALSE)

##### Cleaning of World Bank Data ####
##### GDP, Literacy Male, Literacy Female #####

# Change Column Names
names(gdp)[3] <- "RealGDP"
names(literacyFemale)[3] <- "LiteracyFemale"
names(literacyMale)[3] <- "LiteracyMale"
names(mortalityUnder5)[5] <- "iso3"
names(mortalityUnder5)[8] <- "mortalityValue"
names(mortalityUnder5)[3] <- "year"

## remove rows with NA values
# set na values to -1
# remove records that have -1

gdpClean <- gdp %>% mutate(RealGDP = replace(RealGDP, is.na(RealGDP), -1)) %>% filter(RealGDP != -1)
literacyFemaleClean <- literacyFemale %>% mutate(LiteracyFemale = replace(LiteracyFemale, is.na(LiteracyFemale), -1))%>% filter(LiteracyFemale != -1)
literacyMaleClean <- literacyMale %>% mutate(LiteracyMale = replace(LiteracyMale, is.na(LiteracyMale), -1)) %>% filter(LiteracyMale != -1)
mortalityUnder5Clean <- mortalityUnder5 %>% mutate(mortalityValue = replace(mortalityValue, is.na(mortalityValue), -1)) %>% filter(mortalityValue != -1)

## drop columns
mortalityColDel <- c("GHO", "PUBLISHSTATE", "REGION", "Display.Value", "Low", "High", "Comments")
mortalityUnder5Clean <- select(mortalityUnder5Clean, -mortalityColDel)
# create list of country codes for table joining

url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

# Filter Tables to Countries Only
# Inner Join with List of Country Codes (if a country and in dataset, keep record)

gdpJoin <- inner_join(iso_codes, gdpClean, by = c("ISO2" = "iso2c"))
literacyFemaleJoin <- inner_join(iso_codes, literacyFemaleClean, by = c("ISO2" = "iso2c"))
literacyMaleJoin <- inner_join(iso_codes, literacyMaleClean, by = c("ISO2" = "iso2c"))
mortalityUnder5Join <- inner_join(iso_codes, mortalityUnder5Clean, by = c("ISO3" = "iso3"))

## Mortality Data by Gender

mortalityFemaleJoin <- filter(mortalityUnder5Join, SEX == "FMLE")
mortalityMaleJoin<- filter(mortalityUnder5Join, SEX == "MLE")
mortalityBTSXJoin <- filter(mortalityUnder5Join, SEX == "BTSX")

## Get a cleaned up table that has the years as attributes

literacyMaleFinal <- spread(literacyMaleJoin, year, LiteracyMale)
literacyFemaleFinal <- spread(literacyFemaleJoin, year, LiteracyFemale)
gdpFinal <- spread(gdpJoin, year , RealGDP)
mortalityFemaleFinal <- spread(mortalityFemaleJoin, year, mortalityValue)
mortalityMaleFinal <- spread(mortalityMaleJoin, year, mortalityValue)
mortalityBTSXFinal <- spread(mortalityBTSXJoin, year, mortalityValue)

## Export Final Data to CSVs

write.csv(literacyMaleFinal, "literacyMaleFinal.csv", row.names = FALSE)
write.csv(literacyFemaleFinal, "literacyFemaleFinal.csv", row.names = FALSE)
write.csv(mortalityBTSXFinal, "mortalityBTSXFinal.csv", row.names = FALSE)
write.csv(mortalityMaleFinal, "mortalityMaleFinal.csv", row.names = FALSE)
write.csv(mortalityFemaleFinal, "mortalityFemaleFinal.csv", row.names = FALSE)
write.csv(gdpFinal, "gdpFinal.csv", row.names = FALSE)

##### Processing for World Map Creation #####

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


##### UI: USER INTERFACE FOR R SHINY (AS OF 4/13) #####

ui <- fluidPage(
  titlePanel("Health and Economic Factors Visualization"),
    fluidRow(
      column(3, wellPanel(  
        h4("Select World Map Options"),
            selectInput("map_factor", "Choose a factor for world map visualization.",
                         c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                          "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                            "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5")
                            ),
                            selectizeInput("year_map", "Choose a year between 1980 and 2018", seq(1980, 2018, 1), selected = 2000),
                            selectInput("scale", "Choose a scale for world map:",
                                        c("Normal","Logarithmic"), selected = "Normal")
      ),
      wellPanel(
        h4("Select Scatterplot Options"),
        selectizeInput("year_scatter", "Choose a year between 1980 and 2018", seq(1980, 2018, 1), selected = 2000),
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
          "Insert Notes to User")
        )
      )),
      column(9, wellPanel( 
        h4("World Map Visualization"),
        plotOutput("mapPlot")),
        # ggiraphOutput("intMapPlot")), for interactive plot
        
        wellPanel(
          h4("Scatterplot Visualization"), 
          ggvisOutput("scatter1")
        )))
  )


##### FUNCTIONS #####

## Functions for World Map

setDataSource <- function(name){
  # returns correct data source based on given name
  if(name=="Female Mortality"){
    return(map.world_joined5)
  }else{
    if(name=="Male Mortality"){
      return(map.world_joined4)
    }else{
      if(name=="GDP"){
        return(map.world_joined)
      }else{
        if(name=="Female Literacy"){
          return(map.world_joined3)
        }else{
          if(name=="Male Literacy"){
            return(map.world_joined2)
          }
        }
      }
    }
  }
}


setContributor <- function(name){
  # returns correct contributor info based on given name
  if(name=="Female Mortality"){
    return("World Health Organization") 
  }else{
    if(name=="Male Mortality"){
      return("World Health Organization")
    }else{
      if(name=="GDP"){
        return("World Bank")
      }else{
        if(name=="Female Literacy"){
          return("World Bank")
        }else{
          if(name=="Male Literacy"){
            return("World Bank")
          }
        }
      }
    }
  }
}


setData_Units <- function(name){
  # returns correct data units info based on name
  if(name=="Female Mortality"){
    return("number of female children under 5 who died per 1000 live births")
  }else{
    if(name=="Male Mortality"){
      return("number of male children under 5 who died per 1000 live births")
    }else{
      if(name=="GDP"){
        return("GDP in US dollars per capita")
      }else{
        if(name=="Female Literacy"){
          return("percent of females who are literate")
        }else{
          if(name=="Male Literacy"){
            return("percent of males who are literate")
          }
        }
      }
    }
  }
}

createWorldMap <- function(name, year_input){
  DataSource = setDataSource(name)
  Contributor = setContributor(name)
  data_units = setData_Units(name)
  
  # Get column corresponding to user specified date and create a boolean column for missing values
  a = as.numeric(which(names(DataSource)==year_input))                        # get the column corresponding to user specified date 
  DataSource$missingD2 = with(DataSource, is.na(DataSource[,a]))              # see which rows are missing data for the specified year
  
  # Apply a logarithmic scale to normalize the color distribution of the world map for GDP
  Selected = DataSource[,a]                                                             # Data for choosen attribute for user selected year
  DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
  DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))         # Apply ln transform to normalize colors on graph
  
  # CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale) based on 
  # type of data set and "spread" of the data altogether
  
  if(name=="GDP"){
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
  
  
  # Get the value to be shown when you hover over the map with the mouse
  DataSource$Country = DataSource$region
  DataSource$Country = paste(DataSource$Country,":",as.integer(DataSource[,a]))
  
  myMap2<-ggplot() +
    geom_polygon_interactive(data = DataSource, aes(x = long, y = lat, group = group, fill = filler, tooltip = Country, data_id = region)) +
    labs(title = paste(year_input,name,sep = " "), subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."), caption = paste("source: ",Contributor)) +
    scale_fill_gradientn(name=name,
                         colours = brewer.pal(5, "RdYlBu"), 
                         na.value = 'white',
                         breaks=c(tick0,tick1,tick2,tick3,tick4,tick5,tick6),
                         labels=c(lab0,lab1,lab2,lab3,lab4,lab5,lab6)) +
    theme(text = element_text(color = "#FFFFFF")
          ,panel.background = element_rect(fill = "#444444")
          ,plot.background = element_rect(fill = "#444444")
          ,panel.grid = element_blank()
          ,plot.title = element_text(size = 30)
          ,plot.subtitle = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "right"
          ,legend.background = element_rect(fill = "#444444")
    )
  # for interactive plot myMap2<- ggiraph(code = print(myMap2),tooltip_offx = 20, tooltip_offy = -10,width_svg = 10,height_svg = 6,zoom_max = 4)
  return(myMap2)
}

## Functions for scatterplot

scatterDataMerge <- function(x, y, year) {
  # joins final data tables on country code for year given
  x_data <- x %>% select(c("ISO2", toString(year))) # selects correct year and all countries
  head(x_data)
  y_data <- y %>% select(c("ISO2", toString(year))) # selects correct year and all countries
  head(y_data)
  joinedTable <- inner_join(x_data, y_data, by = c("ISO2" = "ISO2"))
  colnames(joinedTable)[2] = "x"
  colnames(joinedTable)[3] = "y"
  joinedTable <- joinedTable %>% mutate(x = replace(x, is.na(x), -1)) %>% filter(x != -1) %>% mutate(y=replace(y, is.na(y), -1)) %>% filter(y != -1)
  return(joinedTable)
}

createScatterplot <- function(data){
  # creates a scatterplot from table with x and y columns
  plot <- data %>%
    ggvis(~x, ~y) %>%
    layer_points() %>%
    add_axis("x", title = "x title here") %>% add_axis("y", title = "y title here") %>%
    set_options(width = 500, height = 500) %>% layer_smooths()
}

##### SERVER (from scratch) #####

server <- function(input, output){
  
## Reactive World Map
  
  # reactive world map year
  Start_year = reactive({
    format(input$year_map)
  })
  
  # setup reactive variable for map factor in UI
  name <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return("GDP")
    if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return("Male Literacy")
    if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return("Female Literacy")
    if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Overall Mortality") # NEEDS IMPLEMENTED
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Female Mortality")
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Male Mortality")
  })

  # create World Map Plot Output
    output$mapPlot <- renderPlot({createWorldMap(name(), Start_year())})
    # output$intMapPlot <- renderggiraph({createWorldMap(name(), Start_year())}) for interactive plot
  
  
## Reactive Scatterplot
  
  # setup reactive variable for x factor in UI
   scatter_x_reactive <- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$x_factor) return(gdpFinal)
     if ("Male Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyMaleFinal)
     if ("Female Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyFemaleFinal)
     if ("Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityBTSXFinal)
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityFemaleFinal)
     if ("Male Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityMaleFinal)
   })
   
   # setup reactive variable for y factor in UI
   scatter_y_reactive<- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return(gdpFinal)
     if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyMaleFinal)
     if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyFemaleFinal)
     if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityBTSXFinal)
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityFemaleFinal)
     if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityMaleFinal)
   })
  
  # year of scatterplot
   
  scatter_year_reactive = reactive({
    format(input$year_scatter)
  })
  
  # reactive data join of scatter
  scatterDataReactive = reactive({
    scatterDataMerge(x = scatter_x_reactive(), y = scatter_y_reactive(), year = scatter_year_reactive())
  })
  
  # create scatter plot output
  vis <- reactive({
    createScatterplot(scatterDataReactive())
  })
  
  # create scatterplot as ggvis object for Shiny UI
  vis %>% bind_shiny("scatter1")
}


##### Run Shiny App #####

shinyApp(ui = ui, server = server)

# run in console with shiny::runApp("app.R")


