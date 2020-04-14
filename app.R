##### LIBRARIES FROM UI #####

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")

##### LIBRARIES FOR SERVER: SCATTERPLOT

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggvis)) install.packages("ggvis", repos = "http://cran.us.r-project.org")


##### DATA PROCESSING (AS OF 4/13) #####

if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD", start = 1950, end = 2018)
write.csv(gdp, 'gdp.csv', row.names = FALSE)

literacyMale <- WDI(country = "all", indicator = "SE.ADT.LITR.MA.ZS", start = 1950, end = 2018)
write.csv(literacyMale, 'literacyMale.csv', row.names = FALSE)

literacyFemale <- WDI(country = "all", indicator = "SE.ADT.LITR.FE.ZS", start = 1950, end = 2018)
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
                            textInput("year_map", "Choose a year between 1950 and 2019:", value = "2018"),
                            selectInput("scale", "Choose a scale for world map:",
                                        c("Normal","Logarithmic"), selected = "Normal")
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
          "<><><><><><><this is a place to add notes><><><><><><>")
        )
      )),
      column(9, wellPanel( 
        h4("World Map Visualization")),
        wellPanel(
          h4("Scatterplot Visualization"), 
          ggvisOutput("scatter1")
        )))
  )


##### FUNCTIONS #####

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

## Reactive Scatterplot
  
  # setup reactive variable for x factor in UI
   scatter_x_reactive <- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$x_factor) return(gdpFinal)
     if ("Male Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyMaleFinal)
     if ("Female Literacy Rate, over 15 years old" %in% input$x_factor) return(literacyFemaleFinal)
     if ("Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortalityBTSXFinal)
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$x_factor) return(mortailityFemaleFinal)
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



