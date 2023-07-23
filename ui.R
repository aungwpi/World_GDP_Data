library(shiny)
library(ggplot2)

dataset <- gdp_data

fluidPage(
  
  titlePanel("GDP per capita - Linear Regression Model"),
  
  sidebarPanel(
    
    #sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
    #            value=min(1000, nrow(dataset)), step=500, round=0),
    selectInput('model','Regression Model', c("Linear Regression","Random Forest Regressor")),
    selectInput('region', 'Select Region', c('All', dataset$Region)),
    
    
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)