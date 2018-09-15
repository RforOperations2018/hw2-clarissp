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
play.load <- play %>%
  # I would take this as an opportunity to remove the rownames column.
  mutate(center_field_distance = as.numeric(center_field_distance),
         goal_post = as.numeric(goal_post),
         council_district = as.factor(council_district),
         neighborhood = as.factor(neighborhood)
  )

#Defining UI for application 
ui <- navbarPage("Pittsburgh Playing Fields NavBar", 
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              # Allows for user to select the y-variable for the plot
                              selectInput(inputId = "council",
                                          label = "Council District:",
                                          choices = sort(unique(play.load$council_district)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("7", "9")
                                          ),
                              #Allows user to select true or false for the lights variable 
                              checkboxGroupInput(inputId = "lights",
                                          label = "Lights:",
                                          choiceNames = list("True", "False"),
                                          choiceValues = list("True", "False")
                              ),
                              #Allows user to change the range for the center field distance variable 
                              sliderInput(inputId = "center",
                                          label = "Center Field Distance:",
                                          min = min(play.load$center_field_distance, na.rm = T),
                                          max = max(play.load$center_field_distance, na.rm = T),
                                          value = c(min(play.load$center_field_distance, na.rm = T), max(play.load$center_field_distance, na.rm = T)),
                                          step = 10),
                              #Creates the reset button
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("barplot"),
                              plotlyOutput("scatterplot"),
                              plotlyOutput("boxplot")
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
    play <- play.load %>%
      filter(center_field_distance >= input$center[1] & center_field_distance <= input$center[2])
    #Allows for the select input to be reactive 
    if (length(input$hood) > 0 ) {
      play <- subset(play, neighborhood %in% input$hood)
    }
    #Allows for check box input to be reactive 
    if (length(input$lights) > 0) {
      play <- subset(play, has_lights %in% input$lights)
    }

    return(play)
  })

  #Bar plot code
  output$barplot <- renderPlotly({
    play <- playInput()
    ggplot(data = play, aes(x = play$field_usage, y =play$center_field_distance)) +
      geom_bar(stat="identity")
  })
  #Scatter plot code
  output$scatterplot <- renderPlotly({
    play <- playInput()
    ggplot(data = play) +
      geom_point(aes(x = play$center_field_distance, y = play$goal_post, color = play$has_lights))
  })
  #Box plot code
  output$boxplot <- renderPlotly({
    play <- playInput()
    ggplot(data = play) +
      geom_boxplot(mapping = aes(x= play$field_usage, y = play$center_field_distance))
  })
  #Data table code
  output$table <- DT::renderDataTable({
    play <- playInput()
    subset(play, select = c(neighborhood,council_district,fire_zone,field_usage,infield_type,has_lights, center_field_distance,goal_post)
    )
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("playingfields", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(playInput(), file) # or you can set row.names = FALSE in this function so I don't get two rowname columns when downloading
    }
  )
  #Allows for the reset button to reset all the different inputs
  observeEvent(input$reset, {
    updateSelectInput(session, "council", selected = c("7", "9"))
    updateCheckboxGroupInput(session, "lights", label = NULL, choices = NULL, selected = c("True","False"))
    updateSliderInput(session, "center", value = c(min(play.load$center_field_distance, na.rm = T), max(play.load$center_field_distance, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)