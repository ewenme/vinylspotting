
# setup -------------------------------------------------------------------

# load packages
library(shiny)
library(shinythemes)
library(shinybusy)
library(anytime)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(discogger)
library(highcharter)

measures <- list(month_added = c("count", "cumulative"),
                 year_added = c("count", "cumulative"),
                 labels = "count", 
                 artists = "count", 
                 formats = "count", 
                 basic_information.year = "count")


# UI ----------------------------------------------------------------------

ui <- navbarPage(
 
  title = "vinylSpotting", theme = shinytheme("cosmo"),
  
  tabPanel(
    title = "app",
    
    add_busy_bar(color = "#FFFFFF"),

    # username input / submit
    fluidRow(
      column(3,
             wellPanel(
               textInput("userName", label = "user", value = "", 
                         placeholder = "enter your Discogs username"
                         ),
               actionButton("go", "submit")
               ),
      
    # other user inputs
    wellPanel(
      dateRangeInput("dateRange", label = "diggin' dates",
                     startview = "year"
                     ),
      selectInput("dimension", label = "diggin' dimensions", 
                  choices = list(
                    `time` = list("month" = "month_added", "year" = "year_added"),
                    `other`= list("labels" = "labels", 
                                  "artists" = "artists",
                                  "formats" = "formats", 
                                  "issue year" = "basic_information.year")
                    )
      ),
      selectInput("measure", label = "how 2 measure", 
                  choices = ""
                  ),
      selectInput("chartType", label = "choose a chart", 
                  choices = c("bar", "column", "line", "spline"),
                  selected = "spline"
                  )
      )
    ),
  
    # chart panel
    column(9,  
      wellPanel(
        highchartOutput("chart", height = "500px")
      ))
   )),

  # about page
  tabPanel("about",
         fluidRow(
           column(6, includeMarkdown("about.Rmd")
                  )
           )
         )
  )


# server -------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    choices <- measures[[input$dimension]]
    updateSelectInput(session, "measure", choices = choices)
  })
  
  
  # retrieve user collection when username submitted
  collection <- eventReactive(input$go, {
    
    # ensure user name is provided
    req(input$userName)
    
    # hit discogs api
    collection_df <- discogs_user_collection(input$userName, simplify_df = TRUE)
    
    # extract result content
    collection_df <- collection_df$content
    
    # fix dates
    collection_df$date_added <- anydate(collection_df$date_added, "%Y-%m-%d")
    
    # add other time vars
    collection_df$month_added <- yearmonth(collection_df$date_added)
    collection_df$year_added <- year(collection_df$date_added)
    
    collection_df
    
    })
  
  # update available date values based on users collection
  observe({
    
    df <- collection()
    
    updateDateRangeInput(session, "dateRange",
                         start = min(df$date_added),
                         end = max(df$date_added),
                         min = min(df$date_added),
                         max = max(df$date_added))
  })
  
  # collection cut by date range
  collection_dated <- reactive({
    
    req(input$dateRange)
    
    dplyr::filter(collection(), between(date_added, input$dateRange[1], input$dateRange[2]))
  })
  
  collection_dim <- reactive({
    
    req(input$dimension)
    
    if (input$dimension %in% c("month_added", "year_added", "basic_information.year")) {
      
      collection_dated() %>% 
        as_tsibble(key = id, index = input$dimension) %>% 
        summarise(count = n()) %>% 
        fill_gaps(count = 0) %>% 
        mutate(cumulative = cumsum(count))
      
    } else if (input$dimension %in% c("labels", "artists", "formats")) {
      
      group = sym(paste0("basic_information.", input$dimension))
      
      collection_dated() %>% 
        unnest(group = !! group, .sep = "_") %>% 
        group_by(group_name) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% 
        top_n(10)
      
    }
    
  })
  
  
  # highchart output dependent on x-axis var picked by user 
  output$chart <- renderHighchart({
    
    # check for username
    req(input$userName)
    
    df <- collection_dim()
    
    # temp cols
    if (inherits(df, "tbl_ts")) {
      df$x <- df[[index_var(df)]]
    } else {
      df$x <- df$group_name
    } 
    
    df$y <- df[[input$measure]]
    
    # make chart
    df %>% 
      hchart(input$chartType, hcaes(x = "x", y = "y"),
             name = "Records", color = "#252525") %>% 
      hc_yAxis(allowDecimals = FALSE, title = list(text = "No. of records")) %>%
      hc_add_theme(hc_theme_smpl())
    
  })

}

shinyApp(ui = ui, server = server)
