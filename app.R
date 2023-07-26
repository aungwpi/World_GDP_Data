library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)

# Load data 
gdp_data <- readRDS("data/gdp_data.rds")

# See above for the definitions of ui and server
ui <- fluidPage(
  
  titlePanel("World GDP per capita for 2023"),
  
  sidebarPanel(
    
    selectInput('model','Regression Model', c("Linear Regression","Random Forest Regressor")),
    selectInput('region', 'Select Region', c('All', gdp_data$Region)),
    titlePanel("Data Overview"),
    
    fluidRow(
      column(10, includeHTML("overview.html"))
      )
    ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Key Learnings", br(),br(), strong("Motivation"), 
                         p("There are many attributes of each country that could be good predictors of its GDP, however, it is more difficult to predict the GDP per capita, which is GDP for given population. 
           This is because a country can have very high GDP per capita but it may have a very small military. This probably just means that it focuses its resources on other development areas
           but has very low budget for the military. Another example would be Singapore, since it is a city-state, the land area is one of the lowest, and on the other end of the spectrum, 
           the United States of America is one of the largest countries. They both are at the top of the GDP per capita list. They are both very rich countries and their citizens are highly 
           education and very productive. Such scenarios make predicting GDP per capita very difficult. Therefore, it makes the case study fun with lots of opportunity for learning."),
                         strong("Linear Regression"),
                p("There are a few models to choose to fit the data. The easiest one is the linear regression model. However, this model is expected to perform poorly because the feature grid plots above showed
                  that not all of the features are linear. The root mean squared errors on the train and test sets are $13907.06 and $9417.50. This proves that the model does not fit the data very well. The expected
                  GDP per capita of a country is plotted against the predicted numbers based on the linear regression model. The gray reference line is when expected is equal to predicted. Any country above this 
                  line is predicted higher and below this line is predicted lower than the measured/expected GDP per capita."),
                strong("Random Forest Regressor"),
                p("Random Forest Regressor 
A random forest regressor model is a great option for data that has non-linear correlations. This model does better with both train and test sets: RMSE on train set is $5152.13 and test set is $8959.48. Plot below 
                  shows that most data fall close to the reference line but there are a few countries that are significantly off."),
                strong("Summary"),
                p("Predicting GDP per capita is indeed challenging. There are quite a few correlated variables that are not linear. Therefore, it is not suprising that the Linear Regression model does a poor job 
                  relative to the Random Forest Regressor. The correlation matrix showed that out of 32 variables, 8 features are useful for fitting models. If there were more features or more countries in the data,
                  the prediction with the Random Forest Regressor could be improved."),
                strong("References"),
                p("[1] Data downloaded from: https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023"),
                p("[2] Correlation: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html"),
                p("[3] https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html"),
                p("[4] https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rowsum"),
                p("[5] Linear Model: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm"),
                p("[6] Random Forest: https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest"))
                )
    )
)


server <- function(input, output) {
  
  select_model <- reactive({
    gdp_data$Region <- as.factor(gdp_data$Region)
    if (input$region != "All") {
      region <- paste0(input$region)
      #region <- paste0("%", input$region, "%")
      gdp_data <- gdp_data %>% 
        filter(grepl(region, Region))
    } else {
      #plotting <- gdp_data
    }
    
    if (input$model == "Linear Regression") {
      return(gdp_data <- gdp_data %>%
               select("GDP_per_capita", "Linear_Model_GDP_per_capita","Region"))
    } else if (input$model == "Random Forest Regressor") {
      return(gdp_data <- gdp_data %>%
               select("GDP_per_capita", "Random_Forest_GDP_per_capita","Region"))
    }})
  
  output$plot <- renderPlot({
    plotting <- select_model()
    p <- ggplot(plotting, aes_string(x=plotting[,1], y=plotting[,2], color=plotting[,3])) + 
      geom_point(size=3) + geom_abline(slope = 1, intercept = 0) + geom_abline(colour= 'gray') +
      labs(x = "GDP ($ per capita", y = "Predicted GDP ($ per capita") +
      theme_minimal() + theme(
        legend.text = element_text(size = 12),   # Increase the font size for legend labels to 12 (change to your desired size)
        legend.title = element_text(size = 14)   # Increase the font size for legend title to 14 (change to your desired size)
      )
    
    print(p)
    
  }, height=700)
  
}

shinyApp(ui = ui, server = server)