library(bslib)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- page_navbar(
  title="Virginia Crash Data",
  underline=T,
  tabPanel("Plot 1",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("slider1", "Date range", "2015-01-01", "2023-01-01", format="yyyy"),
        selectInput("countyInput", "Select County", c("County1", "County2")),
        checkboxGroupInput("pAffect", "Select Injured/Killed", c("Injured", "Killed"))
      ),
      mainPanel(plotOutput("plot1"), fluidRow())
    )
  ),
  tabPanel("Plot 2",
    sidebarLayout(
      sidebarPanel(
        sliderInput("slider1", "Date range", 2015, 2023, 1, value=2015)
      ),
      mainPanel(plotOutput("plot2"), fluidRow())
    )
  ),
  tabPanel("Plot 3",
    sidebarLayout(
      sidebarPanel(
        sliderInput("slider1", "Date range", 2015, 2023, 1, value=2015)
      ),
      mainPanel(plotOutput("plot3"), fluidRow())
    )
  ),
  footer=tags$footer(
    style = "background-color: #2C3E50; padding: 0px; text-align: center; 
      position: fixed; bottom: 0; width: 100%; color: white", 
    "DS 2003 \u00A0 | \u00A0 Group 10 \u00A0 | \u00A0 Gabe Silverstein, Saarthak Gupta, Hasita Nalluri"),
  theme=bs_theme(
    bootswatch="flatly",
    primary="#18BC9C",
    success="#2C3E50")
)

# Define server logic required to draw plots
server <- function(input, output) {
    output$plot1 <- renderPlot({
      ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
    })
    
    output$plot2 <- renderPlot({
      ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
    })
    
    output$plot3 <- renderPlot({
      ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
