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


####-------------------------------------------- GGPLOT style map ----------------------------------------------#####

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
Start_year = 2010

# User Specified DataSource
#name = "Female Mortality"
#name = "Male Mortality"
name = "GDP"
#name = "Female Literacy"
#name = "Male Literacy"

# Get DataSource from the Name of Plot
if(name=="Female Mortality"){
  DataSource = map.world_joined5
}else{
  if(name=="Male Mortality"){
    DataSource = map.world_joined4
  }else{
    if(name=="GDP"){
      DataSource = map.world_joined
    }else{
      if(name=="Female Literacy"){
        DataSource = map.world_joined3
      }else{
        if(name=="Male Literacy"){
          DataSource = map.world_joined2
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
DataSource$lnfactors <- with(DataSource, pmin(log(1.5+DataSource$factors),.55))  # Apply ln transform to normalize colors on graph

# Generate a plot of the World choosing what chlorpleth data you want to see

# CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT
#filler = factor(DataSource[,a])                     # Get color coated map of World GDP 
#filler = DataSource$factors                         # Get gradient map of World GDP as a percentage of highest GDP
filler = DataSource$lnfactors                       # Get gradient map of World GDP normalized to show differences


myMap<-ggplot() +
  geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler)) +
  labs(title = paste(name,"Data","for",Start_year,sep = " "),subtitle = "source: WORLD BANK") +
  theme(text = element_text(color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

myMap
