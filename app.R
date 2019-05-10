
# setup -------------------------------------------------------------------

# load packages
library(shiny)
library(shinythemes)
library(shinybusy)
library(anytime)
library(dplyr)
library(tsibble)
library(discogger)
library(highcharter)

library(tidyverse)
require(lubridate)
library(zoo)

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
# collection_data %>%
#   as_tsibble(key = id, index = date_added) %>% 
#   index_by(year_month = yearmonth(date_added)) %>%
#   summarise(n_recs = n()) %>%
#   fill_gaps(n_recs = 0) %>%
#   mutate(cum_recs = cumsum(n_recs), ma_recs = slide_dbl(n_recs, mean, .size = 3))


# labels <- unnest(collection_data, basic_information.labels, .sep = "_") %>% 
#   select(instance_id:id, basic_information.labels_name:basic_information.labels_entity_type_name)
# 
# artists <- unnest(collection_data, basic_information.artists, .sep = "_") %>% 
#   select(instance_id:id, basic_information.artists_join:basic_information.artists_id)
# 
# formats <- unnest(collection_data, basic_information.formats, .sep = "_") %>% 
#   select(instance_id:id, basic_information.formats_descriptions:basic_information.formats_text)


# UI ----------------------------------------------------------------------

ui <- navbarPage(
 
  title = "vinylSpotting", theme = shinytheme("cosmo"),
  
  tabPanel(
    title = "app",
    
    add_busy_bar(color = "#FFFFFF"),

    # bit with the username input and submit button 
    fluidRow(
      column(3,
             wellPanel(
               textInput("userName", label = "user", value = "", 
                         placeholder = "enter your Discogs username"
                         ),
               actionButton("go", "submit")
               ),
      
    # bit with the years slider and x-axis var selector
    wellPanel(
      dateRangeInput("dateRange", label = "diggin' dates",
                     startview = "year"
                     ),
      selectInput("xVar", label = "wot 2 look at?", 
                  choices = c("labels", "artists", "formats", "issue year"),
                  selected = "labels"
      ),
      selectInput("chartType", label = "chart type", 
                  choices = c("bar", "column", "line", "spline"),
                  selected = "spline"
                  )
      )
    ),
  
    # Main panel w/ plot
    column(9,  
      wellPanel(
        highchartOutput("chart", height = "500px")
      ))
   )),

  # about page (draws from Rmd file)
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
    
    collection_df
    
    })
  
  collection_dated <- reactive({
    
    filter(collection(), between(date_added, input$dateRange[1], input$dateRange[2]))
  })

  # make dataframe for cumulative line graph
  collection_yearmon <- reactive({
    
    collection() %>%
      as_tsibble(key = id, index = date_added) %>% 
      index_by(year_month = yearmonth(date_added)) %>% 
      summarise(n_recs = n()) %>% 
      fill_gaps(n_recs = 0)  %>% 
      mutate(cum_recs = cumsum(n_recs))
    
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
  
  # highchart output dependent on x-axis var picked by user 
  output$chart <- renderHighchart({
    
    collection_yearmon() %>%
      hchart(input$chartType, hcaes(x = year_month, y = n_recs), name="Records", 
             color="#252525") %>%
      hc_yAxis(allowDecimals = FALSE, title = list(text = "No. of records")) %>%
      hc_add_theme(hc_theme_smpl())
  })

}

shinyApp(ui = ui, server = server)
