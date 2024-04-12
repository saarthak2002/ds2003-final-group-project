library(bslib)
library(ggplot2)
library(shiny)
library(tidyverse)
library(leaflet)
library("shinyWidgets")

dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

translate_crash_severity_to_code <- function(severity_name) {
  severity_code <- switch(severity_name,
                          "Property Damage" = "O",
                          "Serious Injury" = "A",
                          "Minor Injury" = "B",
                          "Possible Injury" = "C",
                          "Fatality" = "K",
                          "All" = "All")
  return(severity_code)
}

translate_code_to_crash_severity <- function(severity_code) {
  severity_meaning <- switch(severity_code,
                             "O" = "Property Damage",
                             "A" = "Serious Injury",
                             "B" = "Minor Injury",
                             "C" = "Possible Injury",
                             "K" = "Fatality",
                             "All" = "All")
  return(severity_meaning)
}

plot_1_data <- read.csv("../plot_1_data.csv")

plot_3_data <- read.csv("../plot_3_data.csv")
weather_conditions <- unique(plot_3_data$Weather.Condition)
weather_conditions <- c("0. Any", weather_conditions)
weather_conditions <- sort(weather_conditions)


ui <- page_navbar(
  title="Virginia Car Crash Data",
  underline=T,
  ####################### PLOT 1 #######################
  tabPanel("Time Series",
           fluidPage(
             # Application title
             titlePanel("Plot 1"),
             mainPanel(
               plotOutput("distPlot1"),
               htmlOutput("dateRangeText"),
               width = "100%"
             ),
             # sidebarLayout(
               layout_columns(
                 card(
                   dateRangeInput2("plot1_dateSlider", h4("Select Date Range"), startview = "year", minview = "months", maxview = "decades", start = "2015-01", end = "2024-01", format = "yyyy-mm"),
                 ),
                 card(
                   selectInput("plot1_countyInput", h4("Select County"), unique(plot_1_data$Physical.Juris.Name)),
                 ),
                 card(
                   checkboxGroupInput("plot1_crashAffect", h4("Select Injured/Killed"), c("Motorists Injured", "Motorists Killed", "Pedestrians Injured", "Pedestrians Killed"), c("Motorists Injured", "Motorists Killed")),
                 ),
                 height = "200px"
                 
               ),
             
             # Debug Info comment out
             textOutput("testingTest"),
               # Sidebar with a slider input
               # sidebarPanel(
                 
               # ),
               
               # Show a plot of the generated distribution
               
             # )
           ) 
  ),
  ######################################################
  
  ####################### PLOT 2 #######################
  tabPanel("Plot 2",
           fluidPage(
             # Application title
             titlePanel("Plot 2"),
             sidebarLayout(
               
               # Sidebar with a slider input
               sidebarPanel(
                 sliderInput("obs2",
                             "Number of observations:",
                             min = 0,
                             max = 1000,
                             value = 500
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot2")
               )
             )
           ) 
  ),
  ######################################################
  
  tabPanel("Charlottesville",
           fluidPage(
             tags$style(HTML("
                #mymap {
                  margin-bottom: 20px;
                }
             ")),
             titlePanel("Dangerous Roads in Charlottesville"),
             leafletOutput("mymap"),
             layout_columns(
               card(
                 "Crash Parameters",
                 sliderTextInput(
                   inputId = "plot3_crashSeverity", 
                   label = "Crash Severity:", 
                   grid = TRUE, 
                   force_edges = TRUE,
                   choices = c(
                     "All",
                     "Property Damage",
                     "Possible Injury",
                     "Minor Injury", 
                     "Serious Injury", 
                     "Fatality"
                   ),
                   width = "100%" 
                 ),
                 sliderInput(
                   "plot3_crashMilitaryTime", 
                   label = "Crash Time", 
                   min = 0, 
                   max = 2400, 
                   value = c(0, 2359), 
                   step=100,
                   width = "100%" 
                 ),
                 layout_columns(
                   checkboxGroupButtons(
                     inputId = "plot3_beltedUnbelted",
                     label = "Belted or Unbelted", 
                     choices = c("Belted", "Unbelted"),
                     selected = c("Belted", "Unbelted"),
                   ),
                   checkboxGroupButtons(
                     inputId = "plot3_alcohol",
                     label = "Alcohol Involved", 
                     choices = c("Yes", "No"),
                     selected = c("Yes", "No"),
                     status = "danger"
                   )
                 ),
               ),
               card(
                 "Weather Conditions",
                 selectInput("plot3_weatherCondition", "Weather", weather_conditions),
                 selectInput("plot3_lightCondition", "Light", weather_conditions),
               ),
               card(
                 "Road Conditions"
               ),
             ),
             textOutput("test31"),
             textOutput("test32"),
             textOutput("test33"),
             textOutput("test34"),
           )
  ),
  
  # footer=tags$footer(
  #   style = "background-color: #2C3E50; padding: 0px; text-align: center; position: fixed; bottom: 0; width: 100%; color: white",
  #   "DS 2003 \u00A0 | \u00A0 Group 10 \u00A0 | \u00A0 Gabe Silverstein, Saarthak Gupta, Hasita Nalluri"
  # ),
  theme=bs_theme(
    bootswatch="flatly",
    primary="#18BC9C",
    success="#2C3E50"
  )
)

# Server logic
server <- function(input, output) {
  
  ####################### PLOT 1 #######################
  plot_1_dynamic_data <- reactive({
    # Read in UI inputs
    county_name <- input$plot1_countyInput
    start_date <- input$plot1_dateSlider[1]
    end_date <- input$plot1_dateSlider[2]
    # Filter data based on selected county, date range
    filtered_data <- plot_1_data %>%
      mutate(MM_YYYY = dmy(paste("01-", MM_YYYY, sep = ""))) %>%
      filter(
        Physical.Juris.Name == county_name,
        MM_YYYY >= ymd(start_date),
        MM_YYYY <= ymd(end_date)
      ) %>%
      # Sort       
      arrange(MM_YYYY)
    
    return(filtered_data)
  })
  
  output$distPlot1 <- renderPlot({
    filtered_data <- plot_1_dynamic_data()
    
    p <- ggplot(filtered_data) +
      scale_color_manual(values = c("Motorists Injured" = "deepskyblue", "Motorists Killed" = "darkred", "Pedestrians Injured" = "dodgerblue4", "Pedestrians Killed" = "coral")) +
      labs(title = paste("Persons Injured Time Series in ", input$plot1_countyInput),
           x = "Date",
           y = "Persons Affected",
           color = "Outcome"
      ) +
      theme_minimal() +
      theme(axis.text=element_text(size=15), legend.text = element_text(size=15))
    
    if("Motorists Injured" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Persons_Injured_sum, color = "Motorists Injured"))
    }
    if("Motorists Killed" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = K_people_sum, color = "Motorists Killed"))
    }
    
    if("Pedestrians Injured" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Pedestrians_Injured_sum, color = "Pedestrians Injured"))
    }
    
    if("Pedestrians Killed" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Pedestrians_Killed_sum, color = "Pedestrians Killed"))
    }
    
    p
  })
  
  output$dateRangeText <- renderText({
    label_start_date <- input$plot1_dateSlider[1]
    label_end_date <- input$plot1_dateSlider[2]
    label_start_month <- month(label_start_date, label = TRUE)
    label_start_year <- year(label_start_date)
    label_end_month <- month(label_end_date, label = TRUE)
    label_end_year <- year(label_end_date)
    
    formatted_text <- paste(
      "Between",
      paste(label_start_month, label_start_year, sep = " "),
      "and",
      paste(label_end_month, label_end_year, sep = " ")
    )
    HTML(paste("<p style='text-align:center; color:#888888;'>", formatted_text, "</p>"))
    
  })
  
  output$testingTest <- renderText(input$plot1_crashAffect)
  ######################################################
  
  ####################### PLOT 2 #######################
  
  output$distPlot2 <- renderPlot({
    hist(rnorm(input$obs2))
  })
  
  ######################################################
  
  plot_3_dynamic_data <- reactive({
    crashSeverity <- translate_crash_severity_to_code(input$plot3_crashSeverity)
    crashMilitaryTimeStart <- input$plot3_crashMilitaryTime[1]
    crashMilitaryTimeEnd <- input$plot3_crashMilitaryTime[2]
    beltedUnbelted <- input$plot3_beltedUnbelted
    alcohol <- input$plot3_alcohol
    
    filtered_data <- plot_3_data
    
    if (crashSeverity != "All") {
      filtered_data <- filtered_data %>%
        filter(Crash.Severity == crashSeverity)
    }
    
    return(filtered_data)
  })
  
  output$mymap <- renderLeaflet({
    
    plot_3_filtered_data <- plot_3_dynamic_data()
    
    leaflet() %>% 
      setView(lat=38.036084, lng=-78.49456, zoom = 14) %>%
      addTiles() %>%
      addMarkers(
        data = plot_3_filtered_data, 
        lng = ~x, 
        lat = ~y, 
        popup = paste(
          "<b>Crash Severity:</b> ", plot_3_filtered_data$Crash.Severity, "<br>",
          "<b>Collision Type:</b> ", plot_3_filtered_data$Collision.Type, "<br>",
          "<b>Persons Injured:</b> ", plot_3_filtered_data$Persons.Injured, "<br>",
          "<b>Persons Killed:</b> ", plot_3_filtered_data$K_People, "<br>",
          "<b>Pedestrians Injured:</b> ", plot_3_filtered_data$Pedestrians.Injured, "<br>",
          "<b>Pedestrians Killed:</b> ", plot_3_filtered_data$Pedestrians.Killed, "<br>",
          "<b>Vehicle Count:</b> ", plot_3_filtered_data$Vehicle.Count, "<br>",
          "<b>Crash Date:</b> ", plot_3_filtered_data$Crash.Date, "<br>",
          "<b>Crash Military Time:</b> ", plot_3_filtered_data$Crash.Military.Time, "<br>",
          "<b>Weather Condition:</b> ", plot_3_filtered_data$Weather.Condition, "<br>",
          "<b>Light Condition:</b> ", plot_3_filtered_data$Light.Condition, "<br>",
          "<b>Roadway Surface Condition:</b> ", plot_3_filtered_data$Roadway.Surface.Condition, "<br>",
          "<b>Roadway Defect:</b> ", plot_3_filtered_data$Roadway.Defect, "<br>",
          "<b>Roadway Description:</b> ", plot_3_filtered_data$Roadway.Description, "<br>",
          "<b>Intersection Type:</b> ", plot_3_filtered_data$Intersection.Type, "<br>",
          "<b>Alcohol Involved:</b> ", plot_3_filtered_data$Alcohol., "<br>",
          "<b>Unrestrained:</b> ", plot_3_filtered_data$Unrestrained., "<br>",
          "<b>Distracted:</b> ", plot_3_filtered_data$Distracted., "<br>",
          "<b>RTE Name:</b> ", plot_3_filtered_data$RTE.Name
        ),
        clusterOptions = markerClusterOptions())
  })
  
  # DEBUG - Comment Out
  output$test31 <- renderText({translate_crash_severity_to_code(input$plot3_crashSeverity)})
  output$test32 <- renderText({input$plot3_crashMilitaryTime})
  output$test33 <- renderText({input$plot3_beltedUnbelted})
  output$test34 <- renderText({input$plot3_alcohol})
  
}

# Complete app with UI and server components
shinyApp(ui, server)
