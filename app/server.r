library(shiny)
library(ggplot2)
library(magrittr)
library(leaflet)

pizza <- jsonlite::fromJSON(
    'FavoriteSpots.json') %>% 
    tidyr::unnest()

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
    
    output$PizzaTable <- DT::renderDataTable(
        pizza,
        rownames=FALSE
    )
    
    selectedStuff <- reactive(
        if(length(input$PizzaTable_rows_selected))
        {
            pizza %>% 
                dplyr::slice(as.numeric(
                    input$PizzaTable_rows_selected
                ))
        } else
        {
            pizza
        }
    )
    
    output$PizzaMap <- renderLeaflet(
        leaflet(
            data=selectedStuff()
        ) %>% 
            addTiles() %>% 
            addMarkers(
                lng= ~ longitude,
                lat= ~ latitude,
                popup= ~ Name
            )
    )
    
})
