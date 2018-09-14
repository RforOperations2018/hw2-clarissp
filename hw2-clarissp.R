library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(shinythemes)
library(stringr)

pdf(NULL)

#Creating variable for arrest data downloaded from the WPRDC 
play <- read.csv("playingfields.csv")
play$center_field_distance <- as.numeric(play$center_field_distance) 

#Defining UI for application 
ui <- navbarPage("Pittsburgh Playing Fields NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Allows for user to select the y-variable for the plot
                              selectInput(inputId = "y",
                                          label = "Y-Axis:",
                                          choices = c("Neighborhood" = "neighborhood",
                                                      "Council District" = "council_district",
                                                      "Fire Zone" = "fire_zone",
                                                      "Field Usage" = "field_usuage",
                                                      "Infield Type" = "infield_type",
                                                      "Has Light?" = "has_lights",
                                                      "Center Field Distance" = "center_field_distance",
                                                      "Number of Goal Posts" = "goal_post"),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = "council_district"
                                          ),
                              #Allows user to select the x-variable for the plot 
                              selectInput(inputId = "x",
                                          label = "X-Axis:",
                                          choices = c("Neighborhood" = "neighborhood",
                                                      "Council District" = "council_district",
                                                      "Fire Zone" = "fire_zone",
                                                      "Field Usage" = "field_usuage",
                                                      "Infield Type" = "infield_type",
                                                      "Has Light?" = "has_lights",
                                                      "Center Field Distance" = "center_field_distance",
                                                      "Number of Goal Posts" = "goal_post"),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = "center_field_distance"
                              ),
                              # Birth Selection
                              sliderInput(inputId = "center_field_distance",
                                          label = "Center Field Distance:",
                                          min = min(play$center_field_distance, na.rm = T),
                                          max = max(play$center_field_distance, na.rm = T),
                                          value = c(min(play$center_field_distance, na.rm = T), max(play$center_field_distance, na.rm = T)),
                                          step = 10),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Pittsburgh Playing Fields Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

#Defining server logic 
server <- function(input, output, session=session) {
  playInput <- reactive({
    #Allows for the slider input to be reactive 
    play <- play %>%
      filter(center_field_distance >= input$center_field_distanceSelect[1] & center_field_distance <= input$center_field_distanceSelect[2])
    #Allows for the select input to be reactive 
    if (length(input$x) > 0 ) {
      play <- subset(play, council_district %in% input$x)
    }
    
    return(play)
  })
  
  output$plot <- renderPlotly({
    ggplot(data = play, aes(x = play$council_district, y =play$center_field_distance)) +
      geom_bar(stat="identity")
  })
  output$table <- DT::renderDataTable({
    play <- playInput()
    subset(play, select = c("Neighborhood" = "neighborhood",
               "Council District" = "council_district",
               "Fire Zone" = "fire_zone",
               "Field Usage" = "field_usuage",
               "Infield Type" = "infield_type",
               "Has Light?" = "has_lights",
               "Center Field Distance" = "center_field_distance",
               "Number of Goal Posts" = "goal_post")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)