# World Map Test file for Server/UI Integration

# Packages for World Map

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org") # interactive labels for plot
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") # interactive labels for plot


# Functions for World Map

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
    labs(title = paste(Start_year,name,sep = " "), subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."), caption = paste("source: ",Contributor)) +
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
  return(myMap2)
}

## Reactive Values from UI Input

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








##### -------------------------- Interactive version of the ggplot Map ------------------------------- #####




# Create an interactive plot with zoom and hover controls with an aspect ratio of 10:6
ggiraph(code = print(myMap2),tooltip_offx = 20, tooltip_offy = -10,width_svg = 10,height_svg = 6,zoom_max = 4)









####################### ARCHIVED CODE EITHER MODIFIED OR MOVED ########################



# setValue <- function(name){
#   # Get DataSource, Contributor and map subtitle from the Name of Plot
#   if(name=="Female Mortality"){
#     DataSource = map.world_joined5
#     Contributor = "World Health Organization"
#     data_units = "number of female children under 5 who died per 1000 live births"
#   }else{
#     if(name=="Male Mortality"){
#       DataSource = map.world_joined4
#       Contributor = "World Health Organization"
#       data_units = "number of male children under 5 who died per 1000 live births"
#     }else{
#       if(name=="GDP"){
#         DataSource = map.world_joined
#         Contributor = "World Bank"
#         data_units = "GDP in US dollars per capita"
#       }else{
#         if(name=="Female Literacy"){
#           DataSource = map.world_joined3
#           Contributor = "World Bank"
#           data_units = "percent of females who are literate"
#         }else{
#           if(name=="Male Literacy"){
#             DataSource = map.world_joined2
#             Contributor = "World Bank"
#             data_units = "percent of males who are literate"
#           }
#         }
#       }
#     }
#   }
# }


# Get User Specified DataSource
#name = "Female Mortality"
#name = "Male Mortality"
#name = "GDP"
#name = "Female Literacy"
#name = "Male Literacy"


# 
# 
# # Get column corresponding to user specified date and create a boolean column for missing values
# a = as.numeric(which(names(DataSource)==Start_year))                        # get the column corresponding to user specified date 
# DataSource$missingD2 = with(DataSource, is.na(DataSource[,a]))              # see which rows are missing data for the specified year


# # Apply a logarithmic scale to normalize the color distribution of the world map for GDP
# Selected = DataSource[,a]                                                             # Data for choosen attribute for user selected year
# DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
# DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))         # Apply ln transform to normalize colors on graph

# CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale) based on 
# type of data set and "spread" of the data altogether
# 
# if(name=="GDP"){
#   filler = DataSource$lnfactors
# }else{
#   filler = DataSource$factors}
# 
# # See what Type of plot you are using -- (Could be replaced by just a name check but who knows if you'll need it later)
# ln = na.omit(ifelse(filler==DataSource$factors,"linear","logarithmic"))
# plotType = ln[1]
# 
# # Calculate the scale to customize the legend used in the Map
# max_n = max(na.omit(filler))
# min_n = min(na.omit(filler))
# range_n = max_n-min_n
# size_ticks = range_n/6
# 
# 
# # Get the units of the tick marks on the legend set
# tick0 = min_n                                                
# tick1 = tick0 + size_ticks
# tick2 = tick1 + size_ticks
# tick3 = tick2 + size_ticks
# tick4 = tick3 + size_ticks
# tick5 = tick4 + size_ticks
# tick6 = max_n
# 
# # Get quartiles for the legend in linear and log format
# if(plotType=="linear"){
#   max_value = as.integer(max(na.omit(DataSource[,a])))           
#   min_value = as.integer(min(na.omit(DataSource[,a])))
#   
#   q1_value = as.integer((1*(max_value-min_value)/6)+min_value)
#   q2_value = as.integer((2*(max_value-min_value)/6)+min_value)
#   q3_value = as.integer((3*(max_value-min_value)/6)+min_value)
#   q4_value = as.integer((4*(max_value-min_value)/6)+min_value)
#   q5_value = as.integer((5*(max_value-min_value)/6)+min_value)
# }
# 
# if(plotType=="logarithmic"){
#   max_value = as.integer(max(na.omit(DataSource[,a]))) 
#   max_ln = log(max_value)
#   min_value = as.integer(min(na.omit(DataSource[,a])))
#   min_ln = log(min_value)
#   diff = max_ln - min_ln
#   
#   q1_value_ln = ((1*(diff)/6)+min_ln)
#   q2_value_ln = ((2*(diff)/6)+min_ln)
#   q3_value_ln = ((3*(diff)/6)+min_ln)
#   q4_value_ln = ((4*(diff)/6)+min_ln)
#   q5_value_ln = ((5*(diff)/6)+min_ln)
#   
#   q1_value = round(exp(q1_value_ln),-2)
#   q2_value = round(exp(q2_value_ln),-2)
#   q3_value = round(exp(q3_value_ln),-3)
#   q4_value = round(exp(q4_value_ln),-3)
#   q5_value = round(exp(q5_value_ln),-3)
#   
#   max_value = round(max_value,-3)
#   min_value = round(min_value,-2)
# }
# 
# #Set labels for the legend ticks
# lab0 = min_value 
# lab1 = q1_value
# lab2 = q2_value
# lab3 = q3_value
# lab4 = q4_value
# lab5 = q5_value
# lab6 = max_value





# Get the value to be shown when you hover over the map with the mouse
# DataSource$Country = DataSource$region
# DataSource$Country = paste(DataSource$Country,":",as.integer(DataSource[,a]))
# 
# myMap2<-ggplot() +
#   geom_polygon_interactive(data = DataSource, aes(x = long, y = lat, group = group, fill = filler, tooltip = Country, data_id = region)) +
#   labs(title = paste(Start_year,name,sep = " "), subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."), caption = paste("source: ",Contributor)) +
#   scale_fill_gradientn(name=name,
#                        colours = brewer.pal(5, "RdYlBu"), 
#                        na.value = 'white',
#                        breaks=c(tick0,tick1,tick2,tick3,tick4,tick5,tick6),
#                        labels=c(lab0,lab1,lab2,lab3,lab4,lab5,lab6)) +
#   theme(text = element_text(color = "#FFFFFF")
#         ,panel.background = element_rect(fill = "#444444")
#         ,plot.background = element_rect(fill = "#444444")
#         ,panel.grid = element_blank()
#         ,plot.title = element_text(size = 30)
#         ,plot.subtitle = element_text(size = 10)
#         ,axis.text = element_blank()
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,legend.position = "right"
#         ,legend.background = element_rect(fill = "#444444")
#   )

