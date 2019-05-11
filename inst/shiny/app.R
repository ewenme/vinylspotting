# setup -------------------------------------------------------------------

# load packages
library(discogger)
library(shiny)
library(shinythemes)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(anytime)
library(dplyr)
library(tidyr)
library(lubridate)
library(tsibble)
library(highcharter)

# set available measures for possible dimensions
measures <- list(month_added = c("count", "cumulative"),
                 year_added = c("count", "cumulative"),
                 labels = "count",
                 artists = "count",
                 formats = "count",
                 basic_information.year = "count")


# UI ----------------------------------------------------------------------

ui <- fluidPage(

  title = "vinylSpotting", theme = shinytheme("cosmo"),

  includeCSS("www/styles.css"),
  useShinyjs(),
  useShinyalert(),
  add_busy_bar(color = "#252525"),

  # username input / submit
  fluidRow(
    column(3,
           h2("vinylSpotting"),
           conditionalPanel(condition = "output.collection_status == false",
                            HTML(paste("<p>Explore your <a href='https://www.discogs.com'>Discogs</a> record collection",
                                       "and buying habits.</p>")),
                            textInput("userName", label = NULL, value = "",
                                      placeholder = "enter your Discogs username"
                                      ),
                            actionButton("go", "submit")
                            ),

           # other user inputs
           conditionalPanel(condition = "output.collection_status == true",
                            dateRangeInput("dateRange", label = "diggin' dates",
                                           startview = "year"
                            ),
                            selectInput("dimension", label = "diggin' dimensions",
                                        choices = list(
                                          `time` = list("month" = "month_added", "year" = "year_added",
                                                        "issue year" = "basic_information.year"),
                                          `other`= list("labels" = "labels",
                                                        "artists" = "artists",
                                                        "formats" = "formats")
                                        )
                            ),
      selectInput("measure", label = "how 2 measure",
                  choices = ""
                  ),
      selectInput("chartType", label = "choose a chart",
                  choices = c("bar", "column", "line", "spline"),
                  selected = "spline"
                  )
      ),
      br(),
      HTML(paste("<p>Made by <a href='https://twitter.com/ewen_'>@ewen_</a>.",
                 "Peep the <a href='https://github.com/ewenme/vinylspotting'>code</a>.</p>"))
    ),

    # chart panel
    column(9,
      conditionalPanel("output.collection_status == true",
        highchartOutput("chart", height = "500px") %>%
          withSpinner(type = 8)
      ))
   )
  )


# server -------------------------------------------------------------

server <- function(input, output, session) {

  # disable submit button if no username entered
  observe({
    toggleState("submit", !is.null(input$userName) && input$userName != "")
  })

  # set measure input choices
  observe({
    choices <- measures[[input$dimension]]
    updateSelectInput(session, "measure", choices = choices)
  })

  # store current collection status
  collection_status <- reactiveValues(ready=NULL)

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

  # success pop-up if collection retrieved
  observe({

    df <- collection()

    # update collection status
    collection_status$ready <- TRUE

    shinyalert(title = "collection found.", type = "success",
               closeOnClickOutside = TRUE, confirmButtonText = "cool",
               timer = 5000, confirmButtonCol = "#000000")
  })

  # show/hide Ui elements depending on collection upload
  output$collection_status <- reactive({
    !is.null(collection_status$ready)
  })
  outputOptions(output, "collection_status", suspendWhenHidden = FALSE)

  # update available date range based on collection data
  observe({

    df <- collection()

    updateDateRangeInput(session, "dateRange",
                         start = min(df$date_added),
                         end = max(df$date_added),
                         min = min(df$date_added),
                         max = max(df$date_added))
  })

  # cut collection by selected date range
  collection_dated <- reactive({

    req(input$dateRange)

    dplyr::filter(collection(), between(date_added, input$dateRange[1], input$dateRange[2]))
  })

  # summarise data based on selected dimension
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

  # chart output
  output$chart <- renderHighchart({

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
      hc_xAxis(title = NULL) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_exporting(enabled = TRUE)

  })

  session$onSessionEnded(stopApp)

}

shinyApp(ui = ui, server = server)
