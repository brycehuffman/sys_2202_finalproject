library(tidyr)
library(WDI)
library(dplyr)
library(magrittr)
library(rvest)
# GGPLOT Style World Map Packages 
library(tidyverse)
library(ggmap)
library(stringr)
library(countrycode)
library(RColorBrewer)
# Leaflet Style World Map Packages 
library(leaflet)
library(jsonlite)
library(rgdal)
library(geojson)

gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD", start = 1950, end = 2018)
write.csv(gdp, 'gdp.csv', row.names = FALSE)

literacyMale <- WDI(country = "all", indicator = "SE.ADT.LITR.MA.ZS", start = 1950, end = 2018)
write.csv(literacyMale, 'literacyMale.csv', row.names = FALSE)

literacyFemale <- WDI(country = "all", indicator = "SE.ADT.LITR.FE.ZS", start = 1950, end = 2018)
write.csv(literacyFemale, 'literacyFemale.csv', row.names = FALSE)

# Pull WHO data from API Query (not functional on 4/6/20)
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

###### --------------------------------------  LEAFLET STYLE MAP ---------------------------------------------######

# Get access to the geojson object from GitHub

json_file <-"https://raw.githubusercontent.com/datasets/geo-boundaries-world-110m/master/countries.geojson"

# Get an R-Studio Version (List of Lists) of the JSON Object downloaded to the computer

geojson <- readLines(json_file, warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

# Get a better looking, solid map with dark lines that is somewhat aesthically pleasing

geojson$style = list(
  weight = 1,
  color = "#555555",
  opacity = 1,
  fillOpacity = 0.8
)

# Access the 177 countries that our geoJSON recognizes and store them into a dataframe

iterations = 177
variables = 2

countries <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  countries[i,1]<-geojson$features[[i]][[2]][[4]]   # Access the name of the country
  countries[i,2]<-geojson$features[[i]][[2]][[45]]  # Access the ISO abbrev. for the country
  print(geojson$features[[i]][[2]][[45]])
}
  
geoCountries <- data.frame(countries)                  # Store that information into a dataframe
colnames(geoCountries) <- c("Country","ISO3")          # Rename the columns of your geoCountries

# Look at Differences between Countries that have been included in each dataframe and geoJSON countries

diff_gdpFinal<-setdiff(geoCountries$ISO3,gdpFinal$ISO3)               # 11 different country names, more than geoJson
diff_mortFemale<-setdiff(geoCountries$ISO3,mortalityFemaleFinal$ISO3) # 10 different country names, more than geoJson
diff_litFemale<-setdiff(geoCountries$ISO3,literacyFemaleFinal$ISO3)   # 32 different country names, less than geoJson

# Now you need to add the Literacy, Child Mortality and GDP data into the geoJSON object 

# Start by Naming the country lists in the JSON object 
newb<-left_join(geoCountries,gdpFinal,by="ISO3")

# Gather GDP estimate from all countries (using geoJSON data object) , store as a numerical vector
gdp_md_est <- sapply(geojson$features, function(feat) {
  feat$properties$gdp_md_est
})

# Gather population estimate from all countries (using geoJSON data object), store as a numerical vector
pop_est <- sapply(geojson$features, function(feat) {
  max(1, feat$properties$pop_est)
})

# Color Map shades of Blue by per-capita GDP using quantiles
pal <- colorQuantile("Blues", gdp_md_est / pop_est)

# Add a properties$style list to each feature
geojson$features <- lapply( geojson$features, function(feat) {
  feat$properties$style <- list(fillColor = pal(feat$properties$gdp_md_est / max(1, feat$properties$pop_est)))
  feat}
)

# Add the now-styled GeoJSON object to the map
map<-leaflet() %>% 
    addGeoJSON(geojson)
    #addPopups(geojson,) get popups to display the country name, gdp, mortality rates and literacy rates.
map


####-------------------------------------------- GGPLOT style map ----------------------------------------------#####

library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(countrycode)

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

# Get User specified Year
Start_year = 1985

# Get User Specified DataSource
#name = "Female Mortality"
#name = "Male Mortality"
name = "GDP"
#name = "Female Literacy"
#name = "Male Literacy"

# Get DataSource, Contributor and map subtitle from the Name of Plot
if(name=="Female Mortality"){
  DataSource = map.world_joined5
  Contributor = "World Health Organization"
  data_units = "number of female children under 5 who died per 1000 live births"
}else{
  if(name=="Male Mortality"){
    DataSource = map.world_joined4
    Contributor = "World Health Organization"
    data_units = "number of male children under 5 who died per 1000 live births"
  }else{
    if(name=="GDP"){
      DataSource = map.world_joined
      Contributor = "World Bank"
      data_units = "GDP in US dollars per capita"
    }else{
      if(name=="Female Literacy"){
        DataSource = map.world_joined3
        Contributor = "World Bank"
        data_units = "percent of females who are literate"
      }else{
        if(name=="Male Literacy"){
          DataSource = map.world_joined2
          Contributor = "World Bank"
          data_units = "percent of males who are literate"
        }
      }
    }
  }
}

# Get a boolean column to see whether or not there is a missing value at the user specified date
a = as.numeric(which(names(DataSource)==Start_year))                        # get the column corresponding to user specified date 
DataSource$missingD2 = with(DataSource, is.na(DataSource[,a]))              # see which rows are missing data for the specified year


# Apply a logarithmic scale to normalize the color distribution of the world map 
Selected = DataSource[,a]                                                             # User selected year
DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))        # Apply ln transform to normalize colors on graph

# CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale)

if(name=="GDP"){
  filler = DataSource$lnfactors
}else{
  filler = DataSource$factors}

ln = na.omit(ifelse(filler==DataSource$factors,"linear","logarithmic"))
plotType = ln[1]

# Calculate the scale to customize the legend used in the Map
max_n = max(na.omit(filler))
min_n = min(na.omit(filler))
range_n = max_n - min_n
size_ticks = range_n/6

# Get the units of the tick marks set
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


# Generate a chloropleth World Map of the User-selected datasets based on attribute and year
myMap<-ggplot() +
  geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler)) +
  labs(title = paste(Start_year,name,sep = " "),subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."),caption = paste("source: ",Contributor)) +
  scale_fill_gradientn(name=name,colours = brewer.pal(5, "RdYlBu"), na.value = 'white',
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

myMap

