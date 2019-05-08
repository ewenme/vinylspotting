
# setup -------------------------------------------------------------------

# load packages
library(shiny)
library(shinythemes)
require(data.table)
library(jsonlite)
library(RCurl)
library(tidyverse)
require(lubridate)
library(highcharter)
library(zoo)
library(discogger)
library(tsibble)

# discogs_api_token()
# 
# username = "hp22"
# 
# data = discogs_user_collection(username, simplify_df = TRUE)
# 
# collection_data <- data$content
# 
# collection_data$date_added <- as.Date(strptime(collection_data$date_added, "%Y-%m-%d"))
# 
# collection_data_df <- as_tsibble(collection_data, key = id, index = date_added)
# 
# collection_data_df %>% 
#   index_by(year_month = yearmonth(date_added)) %>% 
#   summarise(n_recs = n()) %>% 
#   fill_gaps(n_recs = 0) %>% 
#   mutate(cum_recs = cumsum(n_recs))
# 
# foo <- unnest(collection_data, basic_information.labels, .sep = ".")

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage("vinylSpotting",
                 theme = shinytheme("cosmo"),
   
   # Application title
   tabPanel("App",
            
   # bit with the username input and submit button 
   fluidRow(
     column(3,
      wellPanel(
        textInput("username", label = "user", value = "", width = NULL, 
                  placeholder = "enter your Discogs username"),
        actionButton("go", "submit")),
      
    # bit with the years slider and x-axis var selector
      wellPanel(
        sliderInput("year", "choose your digging years", min = 2000, 
                    max = as.integer(format(Sys.Date(), "%Y")),
                    value = c(2000, as.integer(format(Sys.Date(), "%Y"))), 
                    step=1, sep = ""),
        selectInput("xvar", "wot 2 look at?", 
                    c("Labels", "Artists", "Record formats", "Release formats", 
                      "Year of issue", "Month bought", "Year bought"),
                    selected = "Labels")
        )),
  
    # Main panel w/ plot
    column(9,  
      wellPanel(
        highchartOutput("plot", height = "500px")
      ))
   )
),

  # about page (draws from Rmd file)
  tabPanel("About",
         fluidRow(
           column(6, includeMarkdown("about.Rmd")
           ))
         )
)


# SERVER -------------------------------------------------------------

server <- function(input, output, session) {
  
  # retrieve user collection when action button pressed
  collection <- eventReactive(input$go, {
    
    # ensure user name is provided
    req(input$username)
    
    # progress bar
    withProgress(message = "Hold tight!",
                 detail = "(...but if your collection is more than a few k, there's enough time to make a tea)",
                 value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                   }
                   
                   collection_df <- discogs_user_collection(input$username, simplify_df = TRUE)
                   
                   collection_df <- collection_df$content

                   # handle data types
                   collection_df$date_added <- as.Date(strptime(collection_df$date_added, "%Y-%m-%d"))
                   
                   collection_df
                   
                 })
  })
  
  # filter collection based on session inputs
  collection_filtered <- reactive({

    filter(collection(), year(date_added) >= input$year[1],
           year(date_added) <= input$year[2]) 
  })
  
  #make dataframe for cumulative line graph
  collection_yearmon <- reactive({
    
    collection() %>%
      as_tsibble(key = id, index = date_added) %>% 
      index_by(year_month = yearmonth(date_added)) %>% 
      summarise(n_recs = n()) %>% 
      fill_gaps(n_recs = 0)  %>% 
      mutate(cum_recs = cumsum(n_recs))
    
  })

  # update available slider values based on years present in users collection
  observe({
    minyear <- min(year(collection()$date_added))
    maxyear <- max(year(collection()$date_added))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "year", min = minyear, max = maxyear,
                      value= c(minyear, maxyear))
  })
  
  # highchart output dependent on x-axis var picked by user 
  output$plot <- renderHighchart({
    
    collection_yearmon() %>%
      hchart("line", hcaes(x = year_month, y = n_recs), name="Records", color="#252525") %>%
      hc_yAxis(allowDecimals=FALSE, title = list(text = "No. of records")) %>%
      hc_add_theme(hc_theme_smpl())
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
