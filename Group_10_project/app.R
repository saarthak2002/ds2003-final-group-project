# Note: Install these packages:
# install.packages("bslib")
# install.packages("leaflet")
# install.packages("shinyWidgets")

library(bslib)
library(ggplot2)
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(shinyWidgets)

####################### Setup #######################
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

levels <- c("O", "C", "B", "A", "K")
colors <- c("red", "orange", "yellow", "blue", "green")
pal <- colorFactor(colors, levels)

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

format_num <- function(x) {
  case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0("~", as.character(floor(x/1e3)), "k"),
    x < 1e9 ~ paste0("~", as.character(floor(x/1e6)), "mil"),
  )
}

plot_1_data <- read.csv("../plot_1_data.csv")

plot_2_data <- read.csv("../plot_2_data.csv")

plot_3_data <- read.csv("../plot_3_data.csv")
collision_type <- c("0. Any", "1. Rear End", "2. Angle", "3. Head On", "4. Sideswipe - Same Direction",
            "5. Sideswipe - Opposite Direction", "6. Fixed Object in Road", "7. Train",
            "8. Non-Collision", "9. Fixed Object - Off Road", "10. Deer", 
            "11. Other Animal", "12. Ped", "13. Bicyclist", "14. Motorcyclist",
            "15. Backed Into", "16. Other", "Not Applicable")
light_conditions <- c("0. Any", "1. Dawn", "2. Daylight", "3. Dusk", "4. Darkness - Road Lighted", "5. Darkness - Road Not Lighted", "6. Darkness - Unknown Road Lighting", "7. Unknown")
weather_conditions <- c("0. Any", "1. No Adverse Condition (Clear/Cloudy)", "3. Fog", "4. Mist", "5. Rain", "6. Snow", "7. Sleet/Hail", "9. Other", "10. Blowing Sand, Soil, Dirt, or Snow", "11. Severe Crosswinds")
intersection_types <- c("0. Any", "1. Not at Intersection", "2. Two Approaches", "3. Three Approaches", "4. Four Approaches", "5. Five-Point, or More", "6. Roundabout")
road_surface_conditions <- c("0. Any", "1. Dry", "2. Wet", "3. Snowy", "4. Icy", "5. Slush", "6. Oil/Other Fluids", "7. Sand, Dirt, Gravel", "8. Other", "9. Water (Standing, Moving)")
roadway_descriptions <- c("0. Any", "1. Two-Way, Not Divided", "2. Two-Way, Divided, Unprotected Median", "3. Two-Way, Divided, Positive Median Barrier", "4. One-Way, Not Divided", "5. Unknown")
roadway_defects <- c("0. Any", "1. No Defects", "2. Holes, Ruts, Bumps", "3. Slick Pavement", "4. Under Repair", "5. Loose Material", "6. Restricted Width", "7. Roadway Obstructed", "8. Edge Pavement Drop Off", "9. Other")
######################################################

ui <- tags$div(page_navbar(
  title="Virginia Car Crash Data",
  underline=T,
  
  ####################### PLOT 1 #######################
  tabPanel("County Level",
    fluidPage(
      titlePanel("Accident Time Series"),
      h5("How have crash outcomes changed over time and across counties? ", style = "color: #808080; text-align: center;"),
      mainPanel(
        plotlyOutput("distPlot1"),
        htmlOutput("dateRangeText"),
        width = "100%"
      ),
      p("Monthly time series of pedestrians or motorists killed or injured in different Virginia counties. Data obtained from VDOT.", style = "color: #808080; font-weight: 100; font-style: italic; font-size: 14px; text-align: right; margin-top: 0.5rem;"),
      layout_columns(
        card(
          dateRangeInput2("plot1_dateSlider", h4("Select Date Range"), startview = "year", minview = "months", maxview = "decades", start = "2015-01", end = "2024-01", format = "yyyy-mm", min = "2015-01", max="2024-01"),
          "Label Months",
          switchInput(
            inputId = "plot1_labelMonths",
            value = FALSE
          )
        ),
        card(
          selectInput("plot1_countyInput", h4("Select County"), unique(plot_1_data$Physical.Juris.Name)),
        ),
        card(
          checkboxGroupInput("plot1_crashAffect", h4("Select Injured/Killed"), c("Motorists Injured", "Motorists Killed", "Pedestrians Injured", "Pedestrians Killed"), c("Motorists Injured", "Motorists Killed")),
        ),
        height = "200px"
      ),
      
      # DEBUG Info comment out
      textOutput("test11"),
      textOutput("test12"),
      textOutput("test13"),
      textOutput("test14"),
      
    ) 
  ),
  ######################################################
  
  ####################### PLOT 2 #######################
  tabPanel("Greater Virginia",
     fluidPage(
       # Application title
       titlePanel("Factors Affecting Crash Severity"),
       h5("How do overspeeding, driver age and alertness, and weather affect crash severity?", style = "color: #808080; text-align: center;"),
       # Show a plot of the generated distribution
       mainPanel(
         plotlyOutput("distPlot2"),
         width = "100%",
         # DEBUG only
         p("Number of crashes across Virginia by severity category and speed, driver, or weather conditions. Data obtained from VDOT.", style = "color: #808080; font-weight: 100; font-style: italic; font-size: 14px; text-align: right; margin-top: 0.5rem;"),
         textOutput("test21"),
         textOutput("test22"),
         textOutput("test23"),
       ),
       layout_columns(
         card(
           numericInput("plot2_maxSpeedDiff", h4("Over Speed Limit by at Least (MPH)"), 0, min = 0, max = 150),
         ),
         card(
           selectInput("plot2_groupByFactor", h4("Driver Condition"), c("None", "Alcohol", "Drowsy", "Distracted")),
           radioButtons("plot2_ageGroup", label = h4("Driver Age Group"),
                        choices = list("All" = 1, "Young" = 2, "Senior" = 3), 
                        selected = 1)
         ),
         card(
           h4("Display Weather Conditions"),
           switchInput(
             inputId = "plot2_weather",
             value = F
           ),
           h4("Y-Axis Scale"),
           switchInput(
             label = "Log transform",
             inputId = "plot2_log",
             value = F
           )
         )
       )
       
     ) 
  ),
  ######################################################
  
  ####################### PLOT 3 #######################
  tabPanel("Charlottesville",
   fluidPage(
     tags$style(HTML("
        #mymap {
          margin-bottom: 20px;
        }
     ")),
     titlePanel("Dangerous Roads in Charlottesville, VA"),
     h5("Which intersections/streets around UVA are dangerous, and what factors lead to severe crashes?", style = "color: #808080; text-align: center;"),
     leafletOutput("mymap"),
     p("Map of crash locations in Charlottesville, VA with environment, road, and crash parameters. Individual marker color shows severity and size shows vehicles involved. Data obtained from VDOT.", style = "color: #808080; font-weight: 100; font-style: italic; font-size: 14px; text-align: center; margin-top: 0.5rem;"),
     layout_columns(
       card(
         h4("Crash Parameters"),
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
           width = "100%",
         ),
         sliderInput(
           "plot3_crashMilitaryTime", 
           label = "Crash Time", 
           min = 0, 
           max = 24, 
           value = c(0, 24),
           post = ":00",
           width = "100%"
         ),
         layout_columns(
           checkboxGroupButtons(
             inputId = "plot3_beltedUnbelted",
             label = "Belted or Unbelted", 
             choices = c("Belted", "Unbelted"),
             selected = c("Belted", "Unbelted"),
             status = "success",
             checkIcon = list(
               yes = icon("ok", lib = "glyphicon"),
               no = icon("remove", lib = "glyphicon")
             )
           ),
           checkboxGroupButtons(
             inputId = "plot3_alcohol",
             label = "Alcohol Involved", 
             choices = c("Yes", "No"),
             selected = c("Yes", "No"),
             status = "success",
             checkIcon = list(
               yes = icon("ok", lib = "glyphicon"),
               no = icon("remove", lib = "glyphicon")
             )
           )
         ),
         selectInput("plot3_collisionType", "Collision Type", collision_type),
       ),
       card(
         h4("Environment Conditions"),
         selectInput("plot3_weatherCondition", "Weather", weather_conditions),
         selectInput("plot3_lightCondition", "Light", light_conditions),
       ),
       card(
         h4("Road Conditions"),
         selectInput("plot3_intersectionType", "Intersection Type", intersection_types),
         selectInput("plot3_roadwayDescription", "Roadway Type", roadway_descriptions),
         selectInput("plot3_roadSurfaceCondition", "Roadway Surface Condition", road_surface_conditions),
         selectInput("plot3_roadwayDefects", "Roadway Defect", roadway_defects),
       ),
     ),

     # DEBUG Info comment out
     textOutput("test31"),
     textOutput("test32"),
     textOutput("test33"),
     textOutput("test34"),
     textOutput("test35"),
     textOutput("test36"),
     textOutput("test37"),
     textOutput("test38"),
     textOutput("test39"),
     textOutput("test310"),
     textOutput("test311"),
   ),
  ),
  ######################################################
  theme = bs_theme(
    bootswatch="flatly",
    primary="#18BC9C",
    success="#2C3E50"
  ),
  tags$head(tags$style
    (HTML('.irs--shiny .irs-bar {
              background: #18BC9C;
              border-top: 1px solid #18BC9C;
              border-bottom: 1px solid #18BC9C;
            }
                            
            .irs--shiny .irs-single { background-color: #18BC9C; }
            
            .irs--shiny .irs-to { background-color: #18BC9C; }
            
            .irs--shiny .irs-from { background-color: #18BC9C; }
            
            [data-toggle=buttons] > .btn-group > .btn input[type=checkbox] {
              opacity: 0;
              position: absolute;
              left: 0;
              bottom: 0;
              width: 100%;
              height: 100%;
              cursor: pointer;
            }
            .btn.checkbtn.btn-success.active {
              background-color: #18BC9C;
            }
            ')
      )
  ),

  tags$script(HTML(
    'const boxes = document.querySelectorAll("[data-toggle=buttons] > .btn-group > .btn");
     for (var i = 0; i < boxes.length; i++) {
       boxes[i].addEventListener("click", function() {
         if (this.className.includes(" active")) {
            this.className = this.className.replace(" active", "");
         }
         else this.className += " active";
       });
     }
    '
  )),
  
  tags$script(HTML(
    "var navElement = document.getElementsByTagName('nav')[0];
    console.log(navElement);
    var newElement = document.createElement('p');
    var newText = document.createTextNode('Group 6');
    newElement.style.color = 'white';
    newElement.style.fontWeight = 'bold';
    newElement.style.padding = '12px';
    newElement.style.width = '100px';
    newElement.style.marginBottom = '0';
    newElement.appendChild(newText);
    navElement.appendChild(newElement);"
  ))
),
  tags$div(
    style = "background-color: #2C3E50; padding: 0px; text-align: center; width: 100%; color: white;",
    "DS 2003 \u00A0 | \u00A0 Group 6 \u00A0 | \u00A0 Gabe Silverstein, Saarthak Gupta, Hasita Nalluri, Cindy Dong"
  )
)

# Server logic
server <- function(input, output, session) {
  
  ####################### PLOT 1 #######################
  observe({
    # Fix
    if (input$plot1_dateSlider[2] <= input$plot1_dateSlider[1]) {
      updateDateRangeInput(session, inputId="plot1_dateSlider", start="2015-01", end="2024-01")
    }
  })
  
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
  
  output$distPlot1 <- renderPlotly({
    filtered_data <- plot_1_dynamic_data()
    
    label_start_date <- input$plot1_dateSlider[1]
    label_end_date <- input$plot1_dateSlider[2]
    label_start_month <- month(label_start_date, label = TRUE)
    label_start_year <- year(label_start_date)
    label_end_month <- month(label_end_date, label = TRUE)
    label_end_year <- year(label_end_date)

    formatted_date_text <- paste(
      paste(label_start_month, label_start_year, sep = " "),
      "-",
      paste(label_end_month, label_end_year, sep = " ")
    )
    
    p <- ggplot(filtered_data) +
      scale_color_manual(values = c("Motorists Injured" = "deepskyblue", "Motorists Killed" = "darkred", "Pedestrians Injured" = "dodgerblue4", "Pedestrians Killed" = "coral")) +
      labs(title = paste("Persons Injured/Killed in", trimws(gsub("[^[:alpha:] ]", "", input$plot1_countyInput)), "Crashes"),
           x = paste0("Date (", formatted_date_text, ")"),
           y = "Persons Affected",
           color = "Outcome"
      ) +
      theme_minimal() +
      theme(axis.text=element_text(size=15), legend.text = element_text(size=15))
    
    if("Motorists Injured" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Persons_Injured_sum, color = "Motorists Injured", 
        text=paste("Date:", format(MM_YYYY, "%B %Y"), "\nInjuries:", Persons_Injured_sum, "\nOutcome: Motorists Injured"), group=1))
    }
    if("Motorists Killed" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = K_people_sum, color = "Motorists Killed",
        text=paste("Date:", format(MM_YYYY, "%B %Y"), "\nDeaths:", K_people_sum, "\nOutcome: Motorists Killed"), group=1))
    }
    
    if("Pedestrians Injured" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Pedestrians_Injured_sum, color = "Pedestrians Injured",
        text=paste("Date:", format(MM_YYYY, "%B %Y"), "\nInjuries:", Pedestrians_Injured_sum, "\nOutcome: Pedestrians Injured"), group=1))
    }
    
    if("Pedestrians Killed" %in% input$plot1_crashAffect) {
      p <- p + geom_line(aes(x = MM_YYYY, y = Pedestrians_Killed_sum, color = "Pedestrians Killed",
        text=paste("Date:", format(MM_YYYY, "%B %Y"), "\nDeaths:", Pedestrians_Killed_sum, "\nOutcome: Pedestrians Killed"), group=1))
    }
    
    if(input$plot1_labelMonths) {
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=10), legend.text = element_text(size=15))
      p <- p + scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
    }
    else {
      p <- p
    }
    ggplotly(p, tooltip="text") %>%
      layout(title = paste("Persons Injured/Killed in", trimws(gsub("[^[:alpha:] ]", "", input$plot1_countyInput)), "Crashes"), font = list(size = 12))
  })
  
  # output$dateRangeText <- renderText({
  #   label_start_date <- input$plot1_dateSlider[1]
  #   label_end_date <- input$plot1_dateSlider[2]
  #   label_start_month <- month(label_start_date, label = TRUE)
  #   label_start_year <- year(label_start_date)
  #   label_end_month <- month(label_end_date, label = TRUE)
  #   label_end_year <- year(label_end_date)
  #   
  #   formatted_text <- paste(
  #     "Between",
  #     paste(label_start_month, label_start_year, sep = " "),
  #     "and",
  #     paste(label_end_month, label_end_year, sep = " ")
  #   )
  #   HTML(paste("<p style='text-align:center; color:#888888;'>", formatted_text, "</p>"))
  #   
  # })
  
  # DEBUG Info Comment Out
  # output$test13 <- renderText(input$plot1_crashAffect)
  # output$test12 <- renderText(input$plot1_countyInput)
  # output$test11 <- renderText(input$plot1_dateSlider)
  # output$test14 <- renderText(input$plot1_labelMonths)
  ######################################################
  
  ####################### PLOT 2 #######################
  
  plot_2_dynamic_data <- reactive({
    
    severity_counts <- plot_2_data
    
    if(input$plot2_maxSpeedDiff > 0) {
      severity_counts <- severity_counts %>%
        filter(Max.Speed.Diff >= input$plot2_maxSpeedDiff)
    }
    
    if(input$plot2_ageGroup != 1) {
      # Young
      if(input$plot2_ageGroup == 2) {
        severity_counts <- severity_counts %>%
          filter(Young. == "Yes")
      }
      # Senior
      if(input$plot2_ageGroup == 3) {
        severity_counts <- severity_counts %>%
          filter(Senior. == "Yes")
      }
    }
    
    if(input$plot2_groupByFactor != "None") {
      if (input$plot2_weather) {
        severity_counts <- severity_counts %>%
          group_by(Crash.Severity, !!sym(input$plot2_groupByFactor), Weather.Condition) %>%
          summarise(count = n())
      }
      else {
        severity_counts <- severity_counts %>%
          group_by(Crash.Severity, !!sym(input$plot2_groupByFactor)) %>%
          summarise(count = n())
      }

    } else {
        if (input$plot2_weather) {
          severity_counts <- severity_counts %>%
            group_by(Crash.Severity, Weather.Condition) %>%
            summarise(count = n())
        }
        else {
          severity_counts <- severity_counts %>%
            group_by(Crash.Severity) %>%
            summarise(count = n())
        }
    }
    return(severity_counts)
  })
  
  output$distPlot2 <- renderPlotly({
    pastel_colors <- c("#FFB6C1", "#E6E6FA", "#FFDAB9", "#FD8A8A", "#98FB98", 
                       "#FFFFE0", "#AFEEEE", "#FFB347", "#C9A0DC", "#FFBBFF",
                       "#87CEEB")
    
    plot2_title <- "Number of Crashes by Severity"
    plot_2_filtred_data <- plot_2_dynamic_data()
    
    if(input$plot2_maxSpeedDiff > 0) {
      plot2_title <- paste(plot2_title, " - Overspeeding (>= ", input$plot2_maxSpeedDiff, " MPH)")
    }
    
    if(input$plot2_ageGroup == 2) {
      plot2_title <- paste(plot2_title, " - Young Drivers ")
    }
    
    if(input$plot2_ageGroup == 3) {
      plot2_title <- paste(plot2_title, " - Senior Drivers ")
    }
    
    for(i in 1:length(plot_2_filtred_data$Crash.Severity)){
      plot_2_filtred_data$Crash.Severity[i] <- translate_code_to_crash_severity(plot_2_filtred_data$Crash.Severity[i])
    }
    
    plot_2_filtred_data$Crash.Severity <- factor(plot_2_filtred_data$Crash.Severity, ordered = T, 
                                          levels = c("Property Damage", "Possible Injury", "Minor Injury", "Serious Injury", "Fatality"))
    
    
    if(input$plot2_groupByFactor != "None") {
      # Alcohol
      if(input$plot2_groupByFactor == "Alcohol") {
        if (input$plot2_weather) {
          p <- ggplot(plot_2_filtred_data) + 
            geom_bar(aes(x = Alcohol, y = count, fill = Weather.Condition, text=paste("Crashes: ", format_num(count), "\nWeather Condition: ", Weather.Condition, "\nAlcohol: ", Alcohol, sep="")),
                     position = "stack", stat = "identity") +
            facet_grid(~Crash.Severity, switch = "x") +
            theme_minimal() + scale_fill_manual(values=pastel_colors) + labs(title = "Crash Severity", y = "Number of Crashes", fill = "Weather Condition") +
            theme(plot.title = element_text(hjust = 0.5, size=11))
          
          if (input$plot2_log) {
            p <- p + scale_y_log10() + ylab("Number of Crashes (Log Transform)")
          }
          
          fig <- ggplotly(p, tooltip = "text")
        }
        else {
          fig <- plot_ly(
            data = plot_2_filtred_data,
            x = ~Crash.Severity,
            y = ~count,
            color = ~Alcohol,
            type = "bar",
            hovertemplate = ~paste(format_num(count), " crashes", "<extra> Alcohol: ", Alcohol, "</extra>", sep="")
          ) %>% layout(legend=list(title=list(text='<b> Alcohol </b>')))
        }
      }
      # Drowsy
      if(input$plot2_groupByFactor == "Drowsy") {
        if (input$plot2_weather) {
          p <- ggplot(plot_2_filtred_data) + 
            geom_bar(aes(x = Drowsy, y = count, fill = Weather.Condition, text=paste("Crashes: ", format_num(count), "\nWeather Condition: ", Weather.Condition, "\nDrowsy: ", Drowsy, sep="")),
                          position = "stack", stat = "identity") +
            facet_grid(~Crash.Severity, switch = "x") +
            theme_minimal() + scale_fill_manual(values=pastel_colors) + labs(title = "Crash Severity", y = "Number of Crashes", fill = "Weather Condition") +
            theme(plot.title = element_text(hjust = 0.5, size=11))
          
          if (input$plot2_log) {
            p <- p + scale_y_log10() + ylab("Number of Crashes (Log Transform)")
          }
          
          fig <- ggplotly(p, tooltip = "text")
          
        }
        else {
          fig <- plot_ly(
            data = plot_2_filtred_data,
            x = ~Crash.Severity,
            y = ~count,
            color = ~Drowsy,
            type = "bar",
            hovertemplate = ~paste(format_num(count), " crashes", "<extra> Drowsy: ", Drowsy, "</extra>", sep="")
          ) %>% layout(legend=list(title=list(text='<b> Drowsy </b>')))
        }
      }
      # Distracted
      if(input$plot2_groupByFactor == "Distracted") {
        if (input$plot2_weather) {
          p <- ggplot(plot_2_filtred_data) + 
            geom_bar(aes(x = Distracted, y = count, fill = Weather.Condition, text=paste("Crashes: ", format_num(count), "\nWeather Condition: ", Weather.Condition, "\nDistracted: ", Distracted, sep="")),
                     position = "stack", stat = "identity") +
            facet_grid(~Crash.Severity, switch = "x") +
            theme_minimal() + scale_fill_manual(values=pastel_colors) + labs(title = "Crash Severity", y = "Number of Crashes", fill = "Weather Condition") +
            theme(plot.title = element_text(hjust = 0.5, size=11))
          
          if (input$plot2_log) {
            p <- p + scale_y_log10() + ylab("Number of Crashes (Log Transform)")
          }
          
          fig <- ggplotly(p, tooltip = "text")
          
        }
        else {
          fig <- plot_ly(
            data = plot_2_filtred_data,
            x = ~Crash.Severity,
            y = ~count,
            color = ~Distracted,
            type = "bar",
            hovertemplate = ~paste(format_num(count), " crashes", "<extra> Distracted: ", Distracted, "</extra>", sep="")
          ) %>% layout(legend=list(title=list(text='<b> Distracted </b>')))
        }
      }
    }
    else {
      # None
      if (input$plot2_weather) {
        fig <- plot_ly(
          data = plot_2_filtred_data,
          x = ~Crash.Severity,
          y = ~count,
          type = "bar",
          hovertemplate = ~paste(Weather.Condition, ": ", format_num(count), " crashes", "<extra></extra>", sep=""),
          color = ~Weather.Condition,
          colors = pastel_colors
        ) %>% layout(barmode = "stack", legend=list(title=list(text='<b> Weather condition </b>')))
      }
      else {
        fig <- plot_ly(
          data = plot_2_filtred_data,
          title = plot2_title,
          x = ~Crash.Severity,
          y = ~count,
          type = "bar",
          hovertemplate = ~paste(format_num(count), " crashes", "<extra></extra>", sep="")
        ) 
      }
      
    }
    
    if (!input$plot2_weather) {
      fig <- fig %>% layout(xaxis = list(title="Crash Severity"), yaxis = list(title="Number of Crashes"), title = plot2_title)
      
      if (input$plot2_log) {
        # Log scale y axis
        fig <- fig %>% layout(yaxis = list(type = "log", title="Number of Crashes (Log Transform)"))
      }
      
    }
    else if (input$plot2_groupByFactor == "None") {
      fig <- fig %>% layout(xaxis = list(title="Crash Severity"), yaxis = list(title="Number of Crashes"))
      
      if (input$plot2_log) {
        # Log scale y axis
        fig <- fig %>% layout(yaxis = list(type = "log", title="Number of Crashes (Log Transform)"))
      }
    }

    fig
  })
  
  # Debug comment out
  # output$test21 <- renderText(input$plot2_maxSpeedDiff)
  # output$test22 <- renderText(input$plot2_groupByFactor)
  
  ######################################################
  
  ####################### PLOT 3 #######################
  plot_3_dynamic_data <- reactive({
    crashSeverity <- translate_crash_severity_to_code(input$plot3_crashSeverity)
    crashMilitaryTimeStart <- input$plot3_crashMilitaryTime[1]*100
    crashMilitaryTimeEnd <- input$plot3_crashMilitaryTime[2]*100
    beltedUnbelted <- input$plot3_beltedUnbelted
    alcohol <- input$plot3_alcohol
    collision_type <- input$plot3_collisionType
    weather_condition <- input$plot3_weatherCondition
    light_condition <- input$plot3_lightCondition
    intersection_type <- input$plot3_intersectionType
    road_surface_condition <- input$plot3_roadSurfaceCondition
    roadway_description <- input$plot3_roadwayDescription
    roadway_defect <- input$plot3_roadwayDefects
    
    filtered_data <- plot_3_data
    
    # Crash severity
    if (crashSeverity != "All") {
      filtered_data <- filtered_data %>%
        filter(Crash.Severity == crashSeverity)
    }
    
    # Time of Day
    filtered_data <- filtered_data %>%
      filter(Crash.Military.Time >= crashMilitaryTimeStart & 
               Crash.Military.Time <= crashMilitaryTimeEnd)
    
    # Alcohol
    filtered_data <- filtered_data %>%
      filter(Alcohol. %in% alcohol)
    
    # Belted/Unbelted
    filtered_data <- filtered_data %>%
      filter(Unrestrained. %in% beltedUnbelted)
    
    if(collision_type != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Collision.Type == collision_type)
    }
    
    if(weather_condition != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Weather.Condition == weather_condition)
    }
    
    if(light_condition != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Light.Condition == light_condition)
    }
    
    if(intersection_type != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Intersection.Type == intersection_type)
    }
    
    if(roadway_description != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Roadway.Description == roadway_description)
    }
    
    if(road_surface_condition != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Roadway.Surface.Condition == road_surface_condition)
    }
    
    if(roadway_defect != "0. Any") {
      filtered_data <- filtered_data %>%
        filter(Roadway.Defect == roadway_defect)
    }
    
    return(filtered_data)
  })
  
  output$mymap <- renderLeaflet({
    
    plot_3_filtered_data <- plot_3_dynamic_data()
    
    leaflet() %>% 
      setView(lat=38.036084, lng=-78.49456, zoom = 14) %>%
      addTiles() %>%
      addCircleMarkers(
        data = plot_3_filtered_data, 
        lng = ~x, 
        lat = ~y,
        color = "black",
        fillColor = ~pal(Crash.Severity),
        radius = ~(Vehicle.Count*3.5),
        stroke = TRUE,
        fillOpacity = 0.9,
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
  # output$test31 <- renderText({translate_crash_severity_to_code(input$plot3_crashSeverity)})
  # output$test32 <- renderText({input$plot3_crashMilitaryTime})
  # output$test33 <- renderText({input$plot3_beltedUnbelted})
  # output$test34 <- renderText({input$plot3_alcohol})
  # output$test35 <- renderText({input$plot3_collisionType})
  # output$test36 <- renderText({input$plot3_weatherCondition})
  # output$test37 <- renderText({input$plot3_lightCondition})
  # output$test38 <- renderText({input$plot3_intersectionType})
  # output$test39 <- renderText({input$plot3_roadwayDescription})
  # output$test310 <- renderText({input$plot3_roadwayDefects})
  # output$test311 <- renderText({input$plot3_roadSurfaceCondition})
  
  ######################################################
  
}

# Complete app with UI and server components
shinyApp(ui, server)
