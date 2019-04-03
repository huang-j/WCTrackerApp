library(shiny)
# library(shinythemes)
library(ggplot2)
library(data.table)
library(dplyr)
library(googlesheets)
# library(plot)
library(shinymaterial)
library(sweetalertR)
library(shinyWidgets)
library(shinycssloaders)
options(spinner.color="#039be5", spinner.type=2, spinner.color.background="#ffffff")
ui <- material_page(
  title = "",
  ## material nav
  include_nav_bar = TRUE,
  nav_bar_fixed = TRUE,
  nav_bar_color = "light-blue darken-3",
  include_fonts = TRUE,
  
  ## headers:
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  ## side nav
  material_side_nav(
    fixed = FALSE,
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Dashboard" = "dashboard",
        "Weight" = "weight-tab",
        "Lifts" = "barbell-tab"
      )
    )
  ),
  ## material parallax -> unsure of how to keep the scrolling with nav panel
  material_parallax(
    image_source = "rise.jpg"
  ),
  # material_side_nav_tab_content(
  #   side_nav_tab_id = "dashboard",
  #   material_row(
  #   )
  # ),
  material_row(
    material_column(width=4,
                    material_card("Goals", depth=1)),
    material_column(width=4,
                    material_card("Weight change", depth=1)),
    material_column(width=4,
                    material_card("Eating Progress", depth=1))
  ),
  material_row(
      material_column(width=4,
                      material_card("Lifting", depth=1)),
      material_column(width=4,
                      material_card("e", depth=1)),
      material_column(width=4,
                      material_card("f", depth=1))
    ),
  material_parallax(
    image_source = "steak.jpg"
  ),
  material_row(
    # material_column(width=6,
    #                 material_card(
    #                   depth = 2,
    #                   material_date_picker(
    #                     "startdate",
    #                     strong("Start Date"))
    #                 )),
    material_column(width = 6,
                    material_card(
                      depth = 2,
                      airDatepickerInput("date",
                                         label = "Date range",
                                         multiple = 2,
                                         minDate = "2018-5-30",
                                         separator = ' to ',
                                         clearButton = TRUE,
                                         autoClose = TRUE,
                                         position = "top right",
                                         update_on = "close",
                                         value = c(Sys.Date() - 30, Sys.Date()))
                    ))
  ),
  material_row(
    material_column(width=4,
        material_card(
          depth = 1,
          plotOutput("weight", height="275px") %>% withSpinner(),
          plotOutput("deltaW", height="125px") %>% withSpinner()
        )),
    material_column(width=4,
                    material_card(
                      depth = 1,
                      plotOutput("calories") %>% withSpinner()
                    )),
    material_column(width=4,
                    material_card(
                      depth = 1,
                      plotOutput("protein") %>% withSpinner()
                    ))
    
  ),
  material_parallax(
    image_source = "barbell.jpg"
  ),
  material_row(
    material_column(width=3,
                    material_card(
                      depth = 1,
                      plotOutput("deadlift1rm") %>% withSpinner()
                    )),
    material_column(width=3,
                    material_card(
                      depth = 1,
                      plotOutput("squat1rm") %>% withSpinner()
                    )),
    material_column(width=3,
                    material_card(
                      depth = 1,
                      plotOutput("bench1rm") %>% withSpinner()
                    )),
    material_column(width=3,
                    material_card(
                      depth = 1,
                      plotOutput("ohp1rm") %>% withSpinner()
                    ))
  )
)
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
  # input$startdate <- Sys.Date()-30
  # input$enddate <- Sys.Date()
  # 
  
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
  output$weight <- renderPlot({
    mw <- melt(subdate()[, .(Date, Change, `Measured Weight`, `10 day Weight`)], id.vars = c("Date", "Change"))
    w <- ggplot(mw, aes(x=Date, y=value, color=variable)) + geom_point(size=.5) + geom_line(size=1.5) +
      labs(y="Weight (lbs)") + theme(legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank())
    # w <- ggplot(w) %>% layout(legend = list(orientation = "h"))
    w
  })
  output$deltaW <- renderPlot({
    dw <- ggplot(subdate(), aes(x=Date, y=Change, color=Change, fill=Change)) + geom_col() + 
      scale_fill_gradient2() + scale_color_gradient2() + theme(legend.position = "bottom", axis.title.x = element_blank())
    # dw <- ggplot(dw) %>% layout(legend = list(orientation = "h"))
    dw
  })
  ## calories (cooommmbo plot)
  output$calories <- renderPlot({
    mc <- melt(subdate(), id.vars = c("Date", "Total Caloric Intake", "10 day Intake"),
               measure.vars = c("Breakfast Calories", "Lunch Calories", "Dinner Calories", "Snack Calories"))
    c <- ggplot(mc, aes(x=Date)) + 
      geom_col(aes(y=value, color=variable, fill=variable), position="dodge2") + 
      geom_point(aes(y=`Total Caloric Intake`),size=.5, color="#619CFF", fill="white", group=1) + geom_line(aes(y=`Total Caloric Intake`), color="#619CFF",group=1, size=1.5) +
      geom_point(aes(y=`10 day Intake`), size=.5,color="#F564E3", fill="white", group=2) + geom_line(aes(y=`10 day Intake`), color="#F564E3", group=2, size=1.5) +
      labs(y="Calories (kcal)") + theme(legend.position = "bottom", legend.title = element_blank())
    # c <- ggplot(c)
    c
  })
  output$protein <- renderPlot({
    gp <- gw[cell_alt == "R18C2", numeric_value]
    p <- ggplot(subdate(), aes(x=Date, y=`Protein Intake`)) + geom_col(position = "dodge2",  color = "#619CFF", fill="#619CFF") +
      labs(y = "Protein (g)") + geom_hline(yintercept = as.numeric(gp), linetype="dashed", color="red")
    # p <- ggplot(p)
    p
  })
}

shinyApp(ui = ui, server = server)