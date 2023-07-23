library(shiny)
library(ggplot2)

function(input, output) {
  
  select_model <- reactive({
    if (input$region != "All") {
      region <- paste0(input$region)
      plotting <- gdp_data %>% filter(grepl(region, Region))
    } else {
      plotting <- gdp_data
    }
      
    if (input$model == "Linear Regression") {
      return(plotting[,c("GDP_per_capita", "Linear_Model_GDP_per_capita")])
    } else if (input$model == "Random Forest Regressor") {
      return(plotting[,c("GDP_per_capita", "Random_Forest_GDP_per_capita")])
    }})

  
  output$plot <- renderPlot({

    p <- ggplot(select_model(), aes_string(x=select_model()[,1], y=select_model()[,2])) + geom_point() +
    geom_abline(slope = 1, intercept = 0) + geom_abline(colour= 'gray') +
      labs(x = "GDP ($ per capita", y = "Predicted GDP ($ per capita")
    
    print(p)
    
  }, height=700)
  
}