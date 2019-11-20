library(shiny)

shinyServer(function(input, output, session){
    
    output$DisplayCountry <- renderText(
        input$CountrySelector
    )
    
    output$CarHist <- renderPlot(
        ggplot(mtcars, 
               aes_string(x=input$CarColumn)
        ) + 
            geom_histogram(bins=input$CarBins)
    )
    
})
