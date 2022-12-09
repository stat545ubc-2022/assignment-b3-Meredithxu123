# Load required packages
library(shiny)
library(tidyverse)
library(DT)
library(colourpicker)
library(shinythemes)

# Import dataset
mdm<- read.csv("menu.csv")
         
category <- mdm %>%
  select(Category) %>%
  group_by(Category) %>%
  mutate(count=n())


# Define UI for my Shiny App
ui <- fluidPage(
    # Feature 1: Alter the overall appearance of my Shiny App using the shinytheme package
    theme =shinytheme("united"),
    navbarPage(
      # Application title
      "Nutrition Facts for McDownald's Menu",
   tabPanel("Navbar 1","This shiny app enable McDownald's lovers who are also on a diet to find the suitable food items to consume",
   # Sidebar with a slider input for number of bins
   br(),
   sidebarPanel(
      # Feature 2: Add an McDonald image to my Shiny App
      tags$img(src = "Image.png", height= "300px", width = "400px"),
      sliderInput("caloriesInput","Calories",0,1880,value = c(200, 500)),
      # Feature 3: Allow the user to search for multiple menu categories at once instead of only a one at a time.
      selectInput("categoriesInput", "Select your favorite categorie(s)", choices = mdm$Category, 
                  selected = c("Breakfast"), 
                  multiple = TRUE),
      # Feature 4: Allow the user decide on the colours of the bars in the plot.
      colourInput("colour", "Choose your favorite colour", "yellow")),
      # Feature 5: Use the DT package to turn a static table into an interactive table, which allows the user to search and adjust the number of entries that could be presented
      mainPanel(
            plotOutput("menu_plot"),
            # Feature 6: Show the number of meal results found whenever the filters change.
            textOutput("sortedinfo"),
            br(),
            DT::dataTableOutput("menu_table"))),
      tabPanel("Navbar 2", "This panel shows how many categories per each McDonald's category",
               mainPanel(
                 plotOutput("category_plot")
               )))
      )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$menuOutput <- renderUI({
    selectInput("categoriesInput", "Categories",
                sort(unique(mdm$Categories)),
                selected = "Breakfast")
  })
  
  filtered_result <- reactive({
    if (is.null(input$categoriesInput)) {
      return(NULL)
    }
    
    mdm %>% filter(Calories > input$caloriesInput[1],
                   Calories < input$caloriesInput[2],)
  })
  
  output$sortedinfo <- renderText({
    paste("We finally found ",(nrow(filtered_result())), "options for you.
    Feel free to look for your interested one ^_^")
  })

  output$menu_plot <-renderPlot({
    filtered_result() %>%
    ggplot(aes(Total.Fat)) +
    geom_histogram(fill=input$colour)
  })

  
  output$menu_table <-
    DT::renderDataTable({
      filtered_result()
    })
  
  output$category_plot <-renderPlot({
    category %>%
    ggplot(aes(Category,count))+
    geom_col(fill="red")+
    theme_classic()+ 
    geom_text(aes(label=count),color="black",size=5)+
    ggtitle("Menu by Each Category")
  })
  
}

shinyApp(ui = ui, server = server)
