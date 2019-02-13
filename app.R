library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(dplyr)
library(googlesheets)
ui <- fluidPage(
  theme=shinytheme("flatly"),
  ## title
  titlePanel("Weight and Caloric Intake"),
  ## sidebar
  sidebarLayout(
    fluid = TRUE,
    ## a slider to select dates
  sidebarPanel(
    style = "position:fixed;width:inherit;",
    # "Inputs",
    width = 3,
    strong("Goal: 155"),
    textOutput("current"),
    textOutput("155"),
    dateRangeInput("date",
                   strong("Date range"),
                   start=Sys.Date()-30,
                   end=Sys.Date(),
                   min="2018-5-30",
                   max=Sys.Date())
  ),
  ## main panel
  mainPanel(title="plots",
            tabsetPanel(
              tabPanel("Weight", value="weight-tab",
                       plotOutput("weight"),
                       plotOutput("deltaW", height="175px")
                       ),
              tabPanel("Nutrition", value="calories-tab",
                       plotOutput("calories"),
                       plotOutput("protein")
              )
              )
           )
  )
)

server <- function(input, output) {
  ## read in googlesheets data
  WC <- gs_title("Weight and Caloric Intact Tracker")
  Data <- WC %>% gs_read(ws = "Data") %>% as.data.table
  Data[, Date := as.Date(Date, "%m/%d/%Y")]
  ## subset of date based off of date range selection
  subdate <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    subData <- Data[Date >= input$date[1] & Date <= input$date[2]]
  })
  theme_set(theme_minimal())
  ## weight plot
  output$weight <- renderPlot({
    mw <- melt(subdate()[, .(Date, Change, `Measured Weight`, `10 day Weight`)], id.vars = c("Date", "Change"))
    # ggplot(subdate(), aes(x=Date, y=`Measured Weight`)) + geom_point(color = "Blue", group=1) + geom_line(color="Blue", group=1) +
    #   geom_point(aes(y=`10 day Weight`), color = "Red", group=2) + geom_line(aes(y=`10 day Weight`), color = "red", group=2) + 
    #   labs(title="Weight", y = "Weight (lbs)") +
    #   theme_classic()
    ggplot(mw, aes(x=Date, y=value, color=variable)) + geom_point() + geom_line(size=1.5) +
      labs(y="Weight (lbs)") + theme(legend.position = "top", legend.title = element_blank())
  })
  output$deltaW <- renderPlot({
    ggplot(subdate(), aes(x=Date, y=Change, color=Change, fill=Change)) + geom_col() + 
      scale_fill_gradient2() + scale_color_gradient2()
  })
  ## calories (cooommmbo plot)
  output$calories <- renderPlot({
    mc <- melt(subdate(), id.vars = c("Date", "Total Caloric Intake", "10 day Intake"),
               measure.vars = c("Breakfast Calories", "Lunch Calories", "Dinner Calories", "Snack Calories"))
    ggplot(mc, aes(x=Date)) + 
      geom_col(aes(y=value, color=variable, fill=variable), position="dodge2") + 
      geom_point(aes(y=`Total Caloric Intake`), color="#619CFF", fill="white", group=1) + geom_line(aes(y=`Total Caloric Intake`), color="#619CFF",group=1, size=1.5) +
      geom_point(aes(y=`10 day Intake`), color="#F564E3", fill="white", group=2) + geom_line(aes(y=`10 day Intake`), color="#F564E3", group=2, size=1.5) +
      labs(y="Calories (kcal)") + theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)