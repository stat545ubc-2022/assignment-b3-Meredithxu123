library(shiny)
library(tidyverse)
library(DT)
library(colourpicker)


bcl <- read.csv("bcl-data.csv")

ui <- fluidPage(
  titlePanel("BC Liquor Store Data"),
  br(),
  
  sidebarPanel(
    sliderInput("priceInput", "Price", 0, 80,
                value = c(25, 40), pre = "$"),
    # Feature 1: allow the user to search for multiple countries at once instead of only a one at a time.
    selectInput("countryInput", "Select your preferred country", choices = bcl$Country, 
                selected = c("CHINA","CANADA"), 
                multiple = TRUE),
    radioButtons("typeInput", "Alcohol Types",
                  choices = c("BEER", "REFRESHMENT",
                              "SPIRITS", "WINE")),
    # Feature 2: allow the user decide on the colours of the bars in the plot.
    colourInput("colour", "Choose colour", "grey")),
    # Feature 3: Show the number of results found whenever the filters change.
    textOutput("sortedinfo"),
    # Feature 4: Use the DT package to turn a static table into an interactive table.
    plotOutput("alcohol_plot"),
    DT::dataTableOutput("alcohol_table"))
    
  
server <- function(input, output) {
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    bcl %>% filter(Price > input$priceInput[1],
                     Price < input$priceInput[2],
                     Type == input$typeInput,
                     Country == input$countryInput)
  })
  
  output$sortedinfo <- renderText({
    paste("We finally found ",(nrow(filtered())), "options for you.
    Feel free to look for your interested one ^_^")
  })
  
  output$alcohol_plot <-renderPlot({
      filtered() %>%
      ggplot(aes(Alcohol_Content)) +
      geom_histogram(fill=input$colour)
    })
  
  output$alcohol_table <-
    DT::renderDataTable({
      filtered()
    })
  
}

shinyApp(ui = ui, server = server)
