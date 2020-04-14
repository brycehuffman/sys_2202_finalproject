##### Test for Scatterplot Data Merge #####

# run data processing file first 

if(!require(dplyr)) install.packages("magrittr", repos = "http://cran.us.r-project.org") # for joining

scatterDataMerge <- function(x, y, year) {
  # joins final data tables on country code for year given
  x_data <- x %>% select(c("ISO2", toString(year))) # selects correct year and all countries
  head(x_data)
  y_data <- y %>% select(c("ISO2", toString(year))) # selects correct year and all countries
  head(y_data)
  joinedTable <- inner_join(x_data, y_data, by = c("ISO2" = "ISO2"))
  colnames(joinedTable)[2] = "x"
  colnames(joinedTable)[3] = "y"
  return(joinedTable)
}


gdpmortfemtest<- scatterDataMerge(gdpFinal, mortalityFemaleFinal, 2015)


# for ggvis plot

if(!require(ggvis)) install.packages("magrittr", repos = "http://cran.us.r-project.org")

createScatterplot <- function(data){
  # creates a scatterplot from table with x and y columns
  plot <- data %>%
    ggvis(~x, ~y) %>%
    layer_points() %>%
    add_axis("x", title = "x title here") %>% add_axis("y", title = "y title here") %>%
  set_options(width = 500, height = 500) %>% layer_smooths()
  return(plot)
}


# x value of scatter
scatter_x_reactive = reactive({
  format(input$x_factor)
})

# y value of scatter
scatter_y_reactive = reactive({
  format(input$y_factor)
})

# year of scatter
scatter_year_reactive = reactive({
  format(input$scatter_year)
})

# reactive data of scatter
scatterDataReactive = reactive({
  scatterDataMerge(x = scatter_x_reactive(), y = scatter_y_reactive(), year = scatter_year_reactive())
})

# create scatter plot output
output$scatter_plot <- renderPlot({
  createScatterplot(scatterDataReactive())
})
