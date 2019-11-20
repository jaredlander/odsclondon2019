library(shiny)

shinyServer(function(input, output, session){
    
    output$DisplayCountry <- renderText(
        input$CountrySelector
    )
    
})
