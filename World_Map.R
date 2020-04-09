####-------------------------------------------- GGPLOT style map ----------------------------------------------#####

library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(countrycode)

# Append ISO3 codes to the map.word data to be able to merge with the other data frames 
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

# Get User input to change the colors of the chloropleth map 

# User specified Year
Start_year = 2017

# User Specified DataSource
#name = "Female Mortality"
name = "Male Mortality"
#name = "GDP"
#name = "Female Literacy"
#name = "Male Literacy"

# Get DataSource from the Name of Plot
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
DataSource$missingD2 <-with(DataSource, is.na(DataSource[,a]))              # see which rows are missing data for the specified year


# Apply a logarithmic scale to normalize the color distribution of the world map 
Selected = DataSource[,a]                                                                   # User selected year
DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
DataSource$lnfactors <- with(DataSource, pmin(log(1.5+DataSource$factors),.6))  # Apply ln transform to normalize colors on graph

# Generate a plot of the World choosing what chlorpleth data you want to see

# CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT
#filler = DataSource$factors                         # Get gradient map of World GDP as a percentage of highest GDP
filler = DataSource$lnfactors                      # Get gradient map of World GDP normalized to show differences::: 
# DO NOT ALLOW LN FOR MALE OR FEMALE LITERACY RATES

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

# Get quartiles for the legend 
max_value = as.integer(max(na.omit(DataSource[,a])))           
min_value = as.integer(min(na.omit(DataSource[,a])))
q2_value = as.integer((1*(max_value-min_value)/3)+min_value)
q3_value = as.integer((2*(max_value-min_value)/3)+min_value)

#Set labels for the legend ticks
lab0 = min_value
lab1 = " "
lab2 = q2_value
lab3 = " "
lab4 = q3_value
lab5 = " "
lab6 = max_value


myMap<-ggplot() +
  geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler)) +
  labs(title = paste(Start_year,name,sep = " "),subtitle = paste(data_units),caption = paste("source: ",Contributor)) +
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

