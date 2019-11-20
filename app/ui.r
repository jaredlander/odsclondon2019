library(shiny)

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

navbarPage(
    title='Flight of the Navigator',
    selected='Country Info',
    tabPanel(
        title='The World',
        'Hello'
    ),
    tabPanel(
        title='Second Page',
        'Your quest begins here'
    ),
    countryPanel
)
