library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(dplyr)
library(googlesheets)
library(plotly)
ui <- navbarPage("Weight and Caloric Intake",
  theme=shinytheme("flatly"),
  # navlistPanel("Weight and Caloric Intake"),
  ## title
  # titlePanel("Weight and Caloric Intake"),
  ## sidebar
  tabPanel("Dashboard",
           fluidRow(),
           fluidRow()
           ),
  tabPanel("Weight",
           sidebarLayout(
             fluid = TRUE,
             ## a slider to select dates
             sidebarPanel(
               # style = "position:fixed;width:inherit;",
               # "Inputs",
               # width = 3,
               wellPanel(fluidRow(
                 column(6, align="center", strong("Goal Weight")),
                 column(4, align="center", textOutput(outputId =  "goal"))
               )),
               wellPanel(fluidRow(
                 column(6, align="center", strong("Distance from Goal")),
                 column(4, align="center", textOutput(outputId = "distance"))
               ))
               ,
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
                                  plotlyOutput("weight"),
                                  plotlyOutput("deltaW", height="175px")
                         ),
                         tabPanel("Nutrition", value="calories-tab",
                                  plotlyOutput("calories")
                         ),
                         tabPanel("Protein", value="protein-tab",
                                  plotlyOutput("protein"))
                       ) ## end tabsetPanel
                      ) ## end main panel
              )##  end sidbarlayout
    ), ## end tabpanel "plots"
    tabPanel("Lift Tracking",
             sidebarLayout(
               fluid = TRUE,
               sidebarPanel(
                 ## weight stuff remove
                 # wellPanel(fluidRow(
                 #   column(6, align="center", strong("Goal Weight")),
                 #   column(4, align="center", textOutput(outputId =  "goal"))
                 # )),
                 # wellPanel(fluidRow(
                 #   column(6, align="center", strong("Distance from Goal")),
                 #   column(4, align="center", textOutput(outputId = "distance"))
                 # )),
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
                           tabPanel("1RM", value="1RM-tab",
                                    plotOutput("1RM")
                           ),
                           tabPanel("Deadlift", value="dl-tab",
                                    plotOutput("deadlift")
                           ),
                           tabPanel("Squat", value="s-tab",
                                    plotOutput("squat")
                           ),
                           tabPanel("Bench", value="b-tab",
                                    plotOutput("bench"))
                         ) ## end tabsetPanel
               ) ## end main panel 
             )),
    tabPanel("Tables")
  ) ## end navbarPage

server <- function(input, output) {
  ## read in googlesheets data
  WC <- gs_title("Weight and Caloric Intact Tracker")
  Data <- WC %>% gs_read(ws = "Data") %>% as.data.table
  Data[, Date := as.Date(Date, "%m/%d/%Y")]
  Wdata <- WC %>% gs_read(ws = "Big3") %>% as.data.table
  Wdata[, Date := as.Date(Date, "%m/%d/%Y")]
  ######### General Data
  gw <- WC %>% gs_read_cellfeed(ws="Dashboard") %>% as.data.table
  output$goal <- renderText({
    gw[cell_alt == "R3C1", value]
  })
  output$distance <- renderText({
    as.numeric(Data[, .SD[.N]][,`10 day Weight`]) - as.numeric(gw[cell_alt=="R3C1", numeric_value])
  })
  
  
  ## subset of date based off of date range selection
  subdate <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    subData <- Data[Date >= input$date[1] & Date <= input$date[2]]
  })
  theme_set(theme_minimal())
  ############ Plots
  ## weight plot
  output$weight <- renderPlotly({
    mw <- melt(subdate()[, .(Date, Change, `Measured Weight`, `10 day Weight`)], id.vars = c("Date", "Change"))
    # ggplot(subdate(), aes(x=Date, y=`Measured Weight`)) + geom_point(color = "Blue", group=1) + geom_line(color="Blue", group=1) +
    #   geom_point(aes(y=`10 day Weight`), color = "Red", group=2) + geom_line(aes(y=`10 day Weight`), color = "red", group=2) + 
    #   labs(title="Weight", y = "Weight (lbs)") 
    #   theme_classic()
    w <- ggplot(mw, aes(x=Date, y=value, color=variable)) + geom_point(size=.5) + geom_line(size=1.5) +
      labs(y="Weight (lbs)") + theme(legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank())
    w <- ggplotly(w) %>% layout(legend = list(orientation = "h"))
    w
  })
  output$deltaW <- renderPlotly({
    dw <- ggplot(subdate(), aes(x=Date, y=Change, color=Change, fill=Change)) + geom_col() + 
      scale_fill_gradient2() + scale_color_gradient2() + theme(legend.position = "bottom", axis.title.x = element_blank())
    dw <- ggplotly(dw) %>% layout(legend = list(orientation = "h"))
    dw
  })
  ## calories (cooommmbo plot)
  output$calories <- renderPlotly({
    mc <- melt(subdate(), id.vars = c("Date", "Total Caloric Intake", "10 day Intake"),
               measure.vars = c("Breakfast Calories", "Lunch Calories", "Dinner Calories", "Snack Calories"))
    c <- ggplot(mc, aes(x=Date)) + 
      geom_col(aes(y=value, color=variable, fill=variable), position="dodge2") + 
      geom_point(aes(y=`Total Caloric Intake`),size=.5, color="#619CFF", fill="white", group=1) + geom_line(aes(y=`Total Caloric Intake`), color="#619CFF",group=1, size=1.5) +
      geom_point(aes(y=`10 day Intake`), size=.5,color="#F564E3", fill="white", group=2) + geom_line(aes(y=`10 day Intake`), color="#F564E3", group=2, size=1.5) +
      labs(y="Calories (kcal)") + theme(legend.position = "bottom", legend.title = element_blank())
    c <- ggplotly(c)
    c
  })
  output$protein <- renderPlotly({
    gp <- gw[cell_alt == "R18C2", numeric_value]
    p <- ggplot(subdate(), aes(x=Date, y=`Protein Intake`)) + geom_col(position = "dodge2",  color = "#619CFF", fill="#619CFF") +
      labs(y = "Protein (g)") + geom_hline(yintercept = as.numeric(gp), linetype="dashed", color="red")
    p <- ggplotly(p)
    p
  })
}

shinyApp(ui = ui, server = server)