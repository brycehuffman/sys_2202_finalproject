#### Run note: this application peaks around 1.3 gb of ram ####

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
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") # interactive labels for plot
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org") # interactive labels for plot
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")


##### DATA PROCESSING #####

if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")

# World Bank data from API Query with WDI Package: GDP DATA
gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD", start = 1980, end = 2018)
write.csv(gdp, 'gdp.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: MALE LITERACY DATA
literacyMale <- WDI(country = "all", indicator = "SE.ADT.LITR.MA.ZS", start = 1980, end = 2018)
write.csv(literacyMale, 'literacyMale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: FEMALE LITERACY DATA
literacyFemale <- WDI(country = "all", indicator = "SE.ADT.LITR.FE.ZS", start = 1980, end = 2018)
write.csv(literacyFemale, 'literacyFemale.csv', row.names = FALSE) # save raw data to csv

# Pull WHO data from API Query: MORTALITY UNDER AGE 5 DATA
mortalityUnder5 <- read.csv(url("https://apps.who.int/gho/athena/api/GHO/MDG_0000000007?format=csv"))
write.csv(mortalityUnder5, 'mortalityUnder5.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: FEMALE LIFE EXPECTANCY DATA 
expectancyFemale <- WDI(country = "all", indicator = "SP.DYN.LE00.FE.IN", start = 1980, end = 2018)
write.csv(expectancyFemale, 'expectancyFemale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: MALE LIFE EXPECTANCY DATA 
expectancyMale <- WDI(country = "all", indicator = "SP.DYN.LE00.MA.IN", start = 1980, end = 2018)
write.csv(expectancyMale, 'expectancyMale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: TOTAL LIFE EXPECTANCY DATA 
expectancyTotal <- WDI(country = "all", indicator = "SP.DYN.LE00.IN", start = 1980, end = 2018)
write.csv(expectancyTotal, 'expectancyTotal.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: ACCESS TO BASIC WATER DATA 
waterAccess <- WDI(country = "all", indicator = "SH.H2O.BASW.ZS", start = 1980, end = 2018)
write.csv(waterAccess, 'waterAccess.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: SERVICES AS % OF GDP DATA 
serviceGDP <- WDI(country = "all", indicator = "NV.SRV.TOTL.ZS", start = 1980, end = 2018)
write.csv(serviceGDP, 'serviceGDP.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: TRADE AS % OF GDP DATA 
tradeGDP <- WDI(country = "all", indicator = "NE.TRD.GNFS.ZS", start = 1980, end = 2018)
write.csv(tradeGDP, 'tradeGDP.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: GDP PER CAPITA DATA 
capitaGDP <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", start = 1980, end = 2018)
write.csv(capitaGDP, 'capitaGDP.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: OVERALL GDP DATA 
overallGDP <- WDI(country = "all", indicator = "NY.GDP.MKTP.KD", start = 1980, end = 2018)
write.csv(overallGDP, 'overallGDP.csv', row.names = FALSE) # save raw data to csv


# Backup Read from CSV (for presentation purposes due to API server outages)
# Rely on API - To only be used during presentation if website is down

# gdp <- read.csv("gdp.csv")
# literacyMale <- read.csv("literacyMale.csv")
# literacyFemale <- read.csv("literacyFemale.csv")
# mortalityUnder5 <- read.csv("mortalityUnder5.csv")
# expectancyFemale <- read.csv("expectancyFemale.csv")
# expectancyMale <- read.csv("expectancyMale.csv")
# expectancyTotal <- read.csv("expectancyTotal.csv")
# waterAccess <- read.csv("waterAccess.csv")
# serviceGDP <- read.csv("serviceGDP.csv")
# tradeGDP <- read.csv("tradeGDP.csv")
# capitaGDP <- read.csv("capitaGDP.csv")
# overallGDP <- read.csv("overallGDP.csv")


##### Cleaning of World Bank Data ####
## GDP, Literacy Male, Literacy Female

# Change Column Names
names(gdp)[3] <- "RealGDP"
names(literacyFemale)[3] <- "LiteracyFemale"
names(literacyMale)[3] <- "LiteracyMale"
names(mortalityUnder5)[5] <- "iso3"
names(mortalityUnder5)[8] <- "mortalityValue"
names(mortalityUnder5)[3] <- "year"
names(expectancyFemale)[3] <- "ExpectancyFemale"
names(expectancyMale)[3] <- "ExpectancyMale"
names(expectancyTotal)[3] <- "ExpectancyTotal"
names(waterAccess)[3] <- "WaterAccess"
names(serviceGDP)[3] <- "ServiceGDP"
names(tradeGDP)[3] <- "TradeGDP"
names(capitaGDP)[3] <- "CapitaGDP"
names(overallGDP)[3] <- "OverallGDP"


## remove rows with NA values
# set na values to -1
# remove records that have -1

gdpClean <- gdp %>% mutate(RealGDP = replace(RealGDP, is.na(RealGDP), -1)) %>% filter(RealGDP != -1)
literacyFemaleClean <- literacyFemale %>% mutate(LiteracyFemale = replace(LiteracyFemale, is.na(LiteracyFemale), -1))%>% filter(LiteracyFemale != -1)
literacyMaleClean <- literacyMale %>% mutate(LiteracyMale = replace(LiteracyMale, is.na(LiteracyMale), -1)) %>% filter(LiteracyMale != -1)
mortalityUnder5Clean <- mortalityUnder5 %>% mutate(mortalityValue = replace(mortalityValue, is.na(mortalityValue), -1)) %>% filter(mortalityValue != -1)
expectancyFemaleClean <- expectancyFemale %>% mutate(ExpectancyFemale = replace(ExpectancyFemale, is.na(ExpectancyFemale), -1)) %>% filter(ExpectancyFemale != -1)
expectancyMaleClean <- expectancyMale %>% mutate(ExpectancyMale = replace(ExpectancyMale, is.na(ExpectancyMale), -1)) %>% filter(ExpectancyMale != -1)
expectancyTotalClean <- expectancyTotal %>% mutate(ExpectancyTotal = replace(ExpectancyTotal, is.na(ExpectancyTotal), -1)) %>% filter(ExpectancyTotal != -1)
waterAccessClean <- waterAccess %>% mutate(WaterAccess = replace(WaterAccess, is.na(WaterAccess), -1)) %>% filter(WaterAccess != -1)
serviceGDPClean <- serviceGDP %>% mutate(ServiceGDP = replace(ServiceGDP, is.na(ServiceGDP), -1)) %>% filter(ServiceGDP != -1)
tradeGDPClean <- tradeGDP %>% mutate(TradeGDP = replace(TradeGDP, is.na(TradeGDP), -1)) %>% filter(TradeGDP != -1)
capitaGDPClean <- capitaGDP %>% mutate(CapitaGDP = replace(CapitaGDP, is.na(CapitaGDP), -1)) %>% filter(CapitaGDP != -1)
overallGDPClean <- overallGDP %>% mutate(OverallGDP = replace(OverallGDP, is.na(OverallGDP), -1)) %>% filter(OverallGDP != -1)


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
expectancyFemaleJoin <- inner_join(iso_codes, expectancyFemaleClean, by = c("ISO2" = "iso2c"))
expectancyMaleJoin <- inner_join(iso_codes, expectancyMaleClean, by = c("ISO2" = "iso2c"))
expectancyTotalJoin <- inner_join(iso_codes, expectancyTotalClean, by = c("ISO2" = "iso2c"))
waterAccessJoin <- inner_join(iso_codes, waterAccessClean, by = c("ISO2" = "iso2c"))
tradeGDPJoin <- inner_join(iso_codes, tradeGDPClean, by = c("ISO2" = "iso2c"))
serviceGDPJoin <- inner_join(iso_codes, serviceGDPClean, by = c("ISO2" = "iso2c"))
capitaGDPJoin <- inner_join(iso_codes, capitaGDPClean, by = c("ISO2" = "iso2c"))
overallGDPJoin <- inner_join(iso_codes, overallGDPClean, by = c("ISO2" = "iso2c"))


## Mortality Data by Gender

mortalityFemaleJoin <- filter(mortalityUnder5Join, SEX == "FMLE") # female
mortalityMaleJoin<- filter(mortalityUnder5Join, SEX == "MLE") # male
mortalityBTSXJoin <- filter(mortalityUnder5Join, SEX == "BTSX") # both sexes

## Get a cleaned up table that has the years as attributes

literacyMaleFinal <- spread(literacyMaleJoin, year, LiteracyMale)
literacyFemaleFinal <- spread(literacyFemaleJoin, year, LiteracyFemale)
gdpFinal <- spread(gdpJoin, year , RealGDP)
mortalityFemaleFinal <- spread(mortalityFemaleJoin, year, mortalityValue)
mortalityMaleFinal <- spread(mortalityMaleJoin, year, mortalityValue)
mortalityBTSXFinal <- spread(mortalityBTSXJoin, year, mortalityValue)
expectancyFemaleFinal <- spread(expectancyFemaleJoin, year, ExpectancyFemale)
expectancyMaleFinal <- spread(expectancyMaleJoin, year, ExpectancyMale)
expectancyTotalFinal <- spread(expectancyTotalJoin, year, ExpectancyTotal)
waterAccessFinal <- spread(waterAccessJoin, year, WaterAccess)
tradeGDPFinal <- spread(tradeGDPJoin, year, TradeGDP)
serviceGDPFinal <- spread(serviceGDPJoin, year, ServiceGDP)
capitaGDPFinal <- spread(capitaGDPJoin, year, CapitaGDP)
overallGDPFinal <- spread(overallGDPJoin, year, OverallGDP)

## Export Final Data to CSVs

write.csv(literacyMaleFinal, "literacyMaleFinal.csv", row.names = FALSE)
write.csv(literacyFemaleFinal, "literacyFemaleFinal.csv", row.names = FALSE)
write.csv(mortalityBTSXFinal, "mortalityBTSXFinal.csv", row.names = FALSE)
write.csv(mortalityMaleFinal, "mortalityMaleFinal.csv", row.names = FALSE)
write.csv(mortalityFemaleFinal, "mortalityFemaleFinal.csv", row.names = FALSE)
write.csv(gdpFinal, "gdpFinal.csv", row.names = FALSE)
write.csv(expectancyFemaleFinal, "expectancyFemaleFinal.csv", row.names = FALSE)
write.csv(expectancyMaleFinal, "expectancyMaleFinal.csv", row.names = FALSE)
write.csv(expectancyTotalFinal, "expectancyTotalFinal.csv", row.names = FALSE)
write.csv(waterAccessFinal, "waterAccessFinal.csv", row.names = FALSE)
write.csv(tradeGDPFinal, "tradeGDPFinal.csv", row.names = FALSE)
write.csv(serviceGDPFinal, "serviceGDPFinal.csv", row.names = FALSE)
write.csv(capitaGDPFinal, "capitaGDPFinal.csv", row.names = FALSE)
write.csv(overallGDPFinal, "overallGDPFinal.csv", row.names = FALSE)

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
map.world_joined6<-left_join(map.world,mortalityBTSXFinal,by=c('ISO'='ISO3'))        # Both Sex Mortality
map.world_joined7<-left_join(map.world,expectancyFemaleFinal,by=c('ISO'='ISO3'))        # Female Life Expectancy
map.world_joined8<-left_join(map.world,expectancyMaleFinal,by=c('ISO'='ISO3'))        # Male Life Expectancy
map.world_joined9<-left_join(map.world,expectancyTotalFinal,by=c('ISO'='ISO3'))        # Total Life Expectancy
map.world_joined10<-left_join(map.world,waterAccessFinal,by=c('ISO'='ISO3'))        # Basic Access to Water
map.world_joined11<-left_join(map.world,tradeGDPFinal,by=c('ISO'='ISO3'))        # Trade GDP
map.world_joined12<-left_join(map.world,serviceGDPFinal,by=c('ISO'='ISO3'))        # Service GDP
map.world_joined13<-left_join(map.world,capitaGDPFinal,by=c('ISO'='ISO3'))        # Capita GDP
map.world_joined14<-left_join(map.world,overallGDPFinal,by=c('ISO'='ISO3'))        # Overall GDP


##### UI: USER INTERFACE FOR R SHINY (AS OF 4/13) #####

ui <- fluidPage(
  titlePanel("Health and Economic Factors Visualization"),
    fluidRow(
      column(4,
             br(),
             br(),
        wellPanel(
               h4("Welcome!"),
               p("Use corresponding option menus for each plot."),
              p("Hover over countries in the world map or points on the scatterplot for more information.")
             ),
        br(),
        wellPanel(  
        h4("Select World Map Options"),
            selectInput("map_factor", "Choose a factor for world map visualization",
                         c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                          "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                            "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5",
                              "Female Life Expectancy at Birth, in years", "Male Life Expectancy at Birth, in years",
                                "Overall Life Expectancy at Birth, in years", "Percentage of Population With Access to Basic Water Services",
                                  "Trade as % of GDP", "Services as % of GDP", "GDP per Capita, in constant 2011 international $",
                                    "Overall GDP, in constant 2010 US Dollars")
                            ),
                            selectizeInput("year_map", "Choose a year between 1980 and 2018", seq(1980, 2018, 1), selected = 2000)
      ),
      br(),
      br(),
      wellPanel(
        h4("Select Scatterplot Options"),
        # selectizeInput("year_map"),
        # selectInput("x_factor", "Choose a factor for the x-axis of the scatter plot",
        #             c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
        #               "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
        #               "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5"),
        #             selected = "Female Literacy Rate, over 15 years old"
        # ),
        
        selectInput("y_factor", "Choose a factor for the y-axis of the scatter plot",
                    c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                      "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                      "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5",
                      "Female Life Expectancy at Birth, in years", "Male Life Expectancy at Birth, in years",
                      "Overall Life Expectancy at Birth, in years", "Percentage of Population With Access to Basic Water Services",
                      "Trade as % of GDP", "Services as % of GDP", "GDP per Capita, in constant 2011 international $",
                      "Overall GDP, in constant 2010 US Dollars"),
                    selected = "Male Literacy Rate, over 15 years old"
        ),
        p("Note: Both the year and x-axis are represented by the factors selected for the World Map."),
        
      ),
      wellPanel(
        h4("Notes"),
        p("Map may take a few moments to load."),
        textOutput("data_source"),
        textOutput("data_source2")
        )
      ),
      column(8, align = "center", wellPanel( 
        h4("World Map Visualization"),
        plotlyOutput("mapPlot", width = 650, height = 406)),
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
          }else{
            if(name=="Overall Mortality"){
              return(map.world_joined6)
            }else{
              if(name=="Female Life Expectancy"){
                return(map.world_joined7)
              }else{
                if(name=="Male Life Expectancy"){
                  return(map.world_joined8)
                }else{
                  if(name=="Overall Life Expectancy"){
                    return(map.world_joined9)
                  }else{
                    if(name=="Basic Access to Water"){
                      return(map.world_joined10)
                    }else{
                      if(name=="Trade GDP"){
                        return(map.world_joined11)
                      }else{
                        if(name=="Service GDP"){
                          return(map.world_joined12)
                        }else{
                          if(name=="GDP per Capita"){
                            return(map.world_joined13)
                          }else{
                            if(name=="Overall GDP"){
                              return(map.world_joined14)
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
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
          }else{
            if(name=="Overall Mortality"){
              return("World Health Organization")
            }else{
              if(name=="Female Life Expectancy"){
                return("World Bank")
              }else{
                if(name=="Male Life Expectancy"){
                  return("World Bank")
                }else{
                  if(name=="General Life Expectancy"){
                    return("World Bank")
                  }else{
                    if(name=="Basic Access to Water"){
                      return("World Bank")
                    }else{
                      if(name=="Trade GDP"){
                        return("World Bank")
                      }else{
                        if(name=="Service GDP"){
                          return("World Bank")
                        }else{
                          if(name=="GDP per Capita"){
                            return("World Bank")
                          }else{
                            if(name=="Overall GDP"){
                              return("World Bank")
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
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
        return("GDP in USD per capita")
      }else{
        if(name=="Female Literacy"){
          return("percent of females who are literate")
        }else{
          if(name=="Male Literacy"){
            return("percent of males who are literate")
          }else{
            if(name=="Overall Mortality"){
              return("number of children under 5 who died per 1000 live births")
            }else{
              if(name=="Female Life Expectancy"){
                return("from birth, projected age females will live to")
              }else{
                if(name=="Male Life Expectancy"){
                  return("from birth, projected age males will life to")
                }else{
                  if(name=="General Life Expectancy"){
                    return("from birth, projected age the population will live to")
                  }else{
                    if(name=="Basic Access to Water"){
                      return("percent of population that uses at least basic drinking water services")
                    }else{
                      if(name=="Trade GDP"){
                        return("percent to which trading makes up a country's GDP")
                      }else{
                        if(name=="Service GDP"){
                          return("percent to which the value added by services makes up a country's GDP")
                        }else{
                          if(name=="GDP per Capita"){
                            return("GDP conversion via purchasing power parity using 2011 international $")
                          }else{
                            if(name=="Overall GDP"){
                              return("GDP in 2010 USD")
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
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
  
  # Apply a logarithmic scale to normalize the color distribution of the world map for GDP
  Selected = DataSource[,a]                                                             # Data for choosen attribute for user selected year
  DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
  DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))         # Apply ln transform to normalize colors on graph
  
  # CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale) based on 
  # type of data set and "spread" of the data altogether
  
  if(name=="GDP"){
    filler = DataSource$lnfactors
    plotType = "logarithmic"
  }else{
    filler = DataSource$factors
    plotType = "linear"}
  
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
  
  # Get septiles for the legend in linear and log format
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
  
  # Plot the Map using lat and long data with a fill set by the filler variable on line 277
  myMap<-ggplot() +
    geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler,text = paste0("Country : ", region, "<br>","Value : ", as.integer(DataSource[,a])))) +
    labs(title = paste(year_input,name,sep = " "),subtitle = paste(data_units,"(",plotType,"scale )") ,caption = paste("source: ",Contributor)) +
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
  
  # Get the plot of the world with a custom tooltip to display country and value
  myMap2 <- ggplotly(myMap,tooltip = "text") %>%
    # get the subtitle and title for the plot
    layout(title = list(text = paste0(year_input," ",name,
                                      '<br>',
                                      '<sup>',
                                      data_units," (",plotType," scale)",
                                      '</sup>')))
  return(myMap2)
}

## Functions for scatterplot

scatterDataMerge <- function(x, y, year) {
  # joins final data tables on country for year given
  x_data <- x %>% select(c("Country", toString(year))) # selects correct year and all countries
  head(x_data)
  y_data <- y %>% select(c("Country", toString(year))) # selects correct year and all countries
  head(y_data)
  joinedTable <- inner_join(x_data, y_data, by = c("Country" = "Country"))
  colnames(joinedTable)[1] = "i"
  colnames(joinedTable)[2] = "x"
  colnames(joinedTable)[3] = "y"
  joinedTable <- joinedTable %>% mutate(x = replace(x, is.na(x), -1)) %>% filter(x != -1) %>% mutate(y=replace(y, is.na(y), -1)) %>% filter(y != -1)
  return(joinedTable)
}

createScatterplot <- function(data, xName, yName){
  # creates a scatterplot from table with x and y columns
  plot <- data %>%
    ggvis(~x, ~y) %>%
    # scale_numeric("x", trans = "log", expand = 0, nice = TRUE) %>%
    layer_points(fillOpacity := 0.45, size := 50) %>% 
    add_axis("x", title = toString(xName), properties = axis_props(title = list(dy = 25))) %>% 
    add_axis("y", title = toString(yName), properties = axis_props(title = list(dy = -30))) %>%
    add_axis("x", orient = "top", ticks = 0, title = toString(paste(yName, "vs.")), properties = axis_props(title = list(fontSize = 14))) %>%
    add_axis("x", orient = "top", ticks = 0, title = toString(xName), properties = axis_props(title = list(fontSize = 14, dy=18))) %>%
    set_options(width = 500, height = 500) %>% layer_smooths() %>%
    set_options(keep_aspect = TRUE) 
  }
    #add_tooltip(function(data) {
     # Datapoint <- data[data, c("i", "x", "y")]
      # paste0(as.character(Datapoint), "test")
  #   add_tooltip(function(data){
  #   paste0("Country: ", "<br>", "X: ", as.character(data$x), "<br>", "Y: ", as.character(data$y))
  # }, "hover")


# all_countries <- isolate(data)
# id <- all_countries[all_countries$ISO2 == x$ISO2, ]


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
    if ("Female Life Expectancy at Birth, in years" %in% input$map_factor) return("Female Life Expectancy")
    if ("Male Life Expectancy at Birth, in years" %in% input$map_factor) return("Male Life Expectancy")
    if ("Overall Life Expectancy at Birth, in years" %in% input$map_factor) return("Overall Life Expectancy")
    if ("Percentage of Population With Access to Basic Water Services" %in% input$map_factor) return("Basic Access to Water")
    if ("Trade as % of GDP" %in% input$map_factor) return("Trade GDP")
    if ("Services as % of GDP" %in% input$map_factor) return("Service GDP")
    if ("GDP per Capita, in constant 2011 international $" %in% input$map_factor) return("GDP per Capita")
    if ("Overall GDP, in constant 2010 USD" %in% input$map_factor) return("Overall GDP")
    
  })

  
  
  
  # create World Map Plot Output
    # output$mapPlot <- renderPlot({createWorldMap(name(), Start_year())})
    # output$intMapPlot <- renderggiraph({createWorldMap(name(), Start_year())}) for interactive plot
     output$mapPlot <- renderPlotly({createWorldMap(name(), Start_year())}) 
     output$data_source <- renderText({paste("Map/Scatter x-axis Datasource: ",setContributor(name()))})
     output$data_source2 <- renderText({paste("Scatter y-axis DataSource: ",scatterContributor())})
     
## Reactive Scatterplot
  
  # setup reactive variable for x factor in UI
  # matches factor displayed from world map
   scatter_x_reactive <- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return(gdpFinal)
     if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyMaleFinal)
     if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyFemaleFinal)
     if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityBTSXFinal)
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityFemaleFinal)
     if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityMaleFinal)
     if ("Female Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyFemaleFinal)
     if ("Male Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyMaleFinal)
     if ("Overall Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyTotalFinal)
     if ("Percentage of Population With Access to Basic Water Services" %in% input$map_factor) return(waterAccessFinal)
     if ("Trade as % of GDP" %in% input$map_factor) return(tradeGDPFinal)
     if ("Services as % of GDP" %in% input$map_factor) return(serviceGDPFinal)
     if ("GDP per Capita, in constant 2011 international $" %in% input$map_factor) return(capitaGDPFinal)
     if ("Overall GDP, in constant 2010 USD" %in% input$map_factor) return(overallGDPFinal)
   })
   
   # setup reactive variable for y factor in UI
   scatter_y_reactive<- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return(gdpFinal)
     if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyMaleFinal)
     if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyFemaleFinal)
     if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityBTSXFinal)
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityFemaleFinal)
     if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityMaleFinal)
     if ("Female Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyFemaleFinal)
     if ("Male Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyMaleFinal)
     if ("Overall Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyTotalFinal)
     if ("Percentage of Population With Access to Basic Water Services" %in% input$y_factor) return(waterAccessFinal)
     if ("Trade as % of GDP" %in% input$y_factor) return(tradeGDPFinal)
     if ("Services as % of GDP" %in% input$y_factor) return(serviceGDPFinal)
     if ("GDP per Capita, in constant 2011 international $" %in% input$y_factor) return(capitaGDPFinal)
     if ("Overall GDP, in constant 2010 USD" %in% input$y_factor) return(overallGDPFinal)
   })
   
   scatterContributor<- reactive({
     if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return("World Bank")
     if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return("World Bank")
     if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return("World Bank")
     if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
     if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
     if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
     if ("Female Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
     if ("Male Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
     if ("Overall Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
     if ("Percentage of Population With Access to Basic Water Services" %in% input$y_factor) return("World Bank")
     if ("Trade as % of GDP" %in% input$y_factor) return("World Bank")
     if ("Services as % of GDP" %in% input$y_factor) return("World Bank")
     if ("GDP per Capita, in constant 2011 international $" %in% input$y_factor) return("World Bank")
     if ("Overall GDP, in constant 2010 USD" %in% input$y_factor) return("World Bank")
   })
   
   
   # setup reactive variable for x axis label in scatterplot
   # matches factor displayed from world map
   scatter_x_reactive_label <- reactive({input$map_factor})
   
   # setup reactive variable for y axis label in scatterplot
   scatter_y_reactive_label <- reactive({input$y_factor})
   
   
  # year of scatterplot, Same year as world map
  scatter_year_reactive = Start_year
  
  # reactive data join of scatter
  scatterDataReactive = reactive({
    scatterDataMerge(x = scatter_x_reactive(), y = scatter_y_reactive(), year = scatter_year_reactive())
  })
  
  # create scatter plot output
  vis <- reactive({
    createScatterplot(scatterDataReactive(), scatter_x_reactive_label(), scatter_y_reactive_label())
  })
  
  # create scatterplot as ggvis object for Shiny UI
  vis %>% bind_shiny("scatter1")
}


##### Run Shiny App #####

shinyApp(ui = ui, server = server)

# run in console with shiny::runApp("app.R")



