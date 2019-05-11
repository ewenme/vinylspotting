
# setup -------------------------------------------------------------------

# load packages
library(shiny)
library(shinythemes)
library(shinybusy)
library(anytime)
library(dplyr)
library(tidyr)
library(tsibble)
library(discogger)
library(highcharter)


# discogs_api_token()
# 
# username = "hp22"
# 
# data = discogs_user_collection(username, simplify_df = TRUE)
# 
# collection_data <- data$content
# 
# collection_data$date_added <- anydate(collection_data$date_added)
# 


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
      selectInput("xVar", label = "diggin' dimensions", 
                  choices = list(
                    "collection evolution" = "cum_recs", "records x month" = "recs_mon" ,
                    "records x year" = "recs_year", "labels" = "basic_information.labels_name", 
                    "artists" = "basic_information.artists_name", 
                    "formats" = "basic_information.formats_name" , 
                    "issue year" = "basic_information.year"
                    )
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
    
    as_tsibble(collection_df, key = id, index = date_added)
    
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
    
    dplyr::filter(collection(), between(date_added, input$dateRange[1], input$dateRange[2]))
  })

  # make time aware df
  collection_year <- reactive({
    
    collection_dated() %>%
      index_by(period = year(date_added)) %>% 
      summarise(recs_year = n()) %>% 
      fill_gaps(recs_year = 0)
    
  })
  
  collection_yearmon <- reactive({
    
    collection_dated() %>%
      index_by(period = yearmonth(date_added)) %>% 
      summarise(recs_mon = n()) %>% 
      fill_gaps(recs_mon = 0)  %>% 
      mutate(cum_recs = cumsum(recs_mon))
    
  })
  
  collection_dim <- reactive({
    
    switch(input$xVar,
           "cum_recs" = collection_yearmon(),
           "records x month" = collection_yearmon(),
           "records x year" = collection_year(),
           "labels" = unnest(collection_dated(), basic_information.labels, .sep = "_"),
           "artists" = unnest(collection_dated(), basic_information.artists, .sep = "_"),
           "formats" = unnest(collection_dated(), basic_information.formats, .sep = "_"),
           "issue year" = collection_dated()
           )
  })
  
  # highchart output dependent on x-axis var picked by user 
  output$chart <- renderHighchart({
    
    # check for username
    req(input$userName)
    
    df <- collection_dim()
    
    # temp col for highcharter
    df$y <- df[[input$xVar]]
    
    # time series chart base
    if ("period" %in% colnames(df)) {
      
      hchart(df, input$chartType, hcaes(x = "period", y = "y"),
             name = "Records", color = "#252525") 
    }
    
    # add generic chart layers
    chart %>%
      hc_yAxis(allowDecimals = FALSE, title = list(text = "No. of records")) %>%
      hc_add_theme(hc_theme_smpl())
  })

}

shinyApp(ui = ui, server = server)
