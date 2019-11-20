library(shiny)
library(shinythemes)

countryPanel <- tabPanel(
    title='Country Info',
    selectInput(
        inputId='CountrySelector',
        label='Choose a Country',
        choices=list(
            'Japan', 'China', 'Peru', 'New Zealand',
            'Ecuador', 'USA', 'Iceland'
        ),
        selected='Ecuador'
    ),
    textOutput(
        outputId='DisplayCountry'
    )
)

plotPanel <- tabPanel(
    title='Simple Plot',
    fluidRow(
        column(
            width=4,
            selectInput(
                inputId='CarColumn',
                label='Choose a Column',
                choices=names(mtcars)
            ),
            sliderInput(
                inputId='CarBins',
                label='Choose number of Bins',
                min=3, max=100, value=30
            )
        ),
        column(
            width=8,
            plotOutput(
                outputId='CarHist'
            )
        )
    )
)

pizzaStuff <- tabPanel(
    title='Pizza',
    fluidRow(
        column(
            width=6,
            DT::dataTableOutput(
                outputId='PizzaTable',
                width='95%'
            )
        ),
        column(
            width=6,
            leaflet::leafletOutput(
                outputId='PizzaMap', 
                width='95%'
            )
        )
    )
)

navbarPage(
    title='Flight of the Navigator',
    selected='Pizza',
    theme=shinytheme('sandstone'),
    themeSelector(),
    tabPanel(
        title='The World',
        'Hello'
    ),
    tabPanel(
        title='Second Page',
        'Your quest begins here'
    ),
    countryPanel,
    plotPanel,
    pizzaStuff
)
