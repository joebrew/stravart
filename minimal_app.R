# Note, the first time you run this (locally), it may error
# This appears to be a bug: https://github.com/rstudio/shiny/issues/1942
# To prevent, just run the following once: 
# options(shiny.port = 8100)

library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(leaflet.extras)
source('global.R')


# Read in credentials
creds <- yaml::yaml.load_file('credentials.yaml')

if(interactive()){
  message('Interactive/testing mode')
  # testing url only
  options(shiny.port = 8100)
  app_url <- creds$url_local
  running_locally <- TRUE
} else {
  app_url <- creds$url_remote
  running_locally <- FALSE
}

# Define oauth2 flow locations, scope, and params
app <- oauth_app(appname = 'GPSArt', 
                 key = creds$client_id, 
                 secret = creds$app_secret,
                 redirect_uri = app_url)
api <- oauth_endpoint(request = "https://www.strava.com/oauth/mobile/authorize?", # consider removing "mobile" if problematic
                      authorize = "https://www.strava.com/oauth/mobile/authorize", # consider removing "mobile" if problematic 
                      access = "https://www.strava.com/oauth/token")

scope <- "activity:read_all"
params <- list(client_id='19335',
               response_type='code',
               redirect_uri= app_url,
               # scope = "activity:read_all"
               approval_prompt='auto')

ui <- fluidPage(theme = shinytheme('united'),
                # Make progress bar large and centered
                tags$head(
                  tags$style(
                    HTML(".shiny-notification {
              height: 150px;
              width: 300px;
              position:fixed;
              top: calc(50% - 75px);;
              left: calc(50% - 300px);;
            }
           "
                    )
                  )),
                sidebarLayout(
                  sidebarPanel(h2('Side stuff')),
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Tabby", 
                                         fluidPage(
                                           fluidRow(column(12, align = 'center', h3(textOutput('development_text')))),
                                           mobileDetect('isMobile'),
                                           textOutput('isItMobile')))))))


uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    # OAuthorize
    # url <- oauth2.0_authorize_url(api, app, scope=scope, query_extra=params)
    # redirect <- sprintf("location.replace(\"%s\");", url)
    uiOutput('auth_submit')
    # tags$script(HTML(redirect))
  } else {
    # Manually create a token
    message('getting token...')
    token <- oauth2.0_token(
      endpoint = api,
      app = app,
      credentials = oauth2.0_access_token(api, app, parseQueryString(req$QUERY_STRING)$code, user_params=params),
      cache = TRUE
    )
    message('saving token...')
    save(token, file = 'token.RData')
    message('done with token retrieval')
    return(ui)
  }
}

server <- function(input, output, session) {
  
  # Mobile phone detection
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "You are on a mobile device", "You are not on a mobile device")
  })
  
  # Button for going to authentication page
  output$auth_submit <- renderUI({
    url <- oauth2.0_authorize_url(api, app, scope=scope, query_extra=params)
    
    fluidPage(
      fluidRow(column(12, align = 'center', h1('GPSart'))),
      fluidRow(column(12, align = 'center', helpText('Beautiful, one-of-a-kind, personalized art prints based on your Strava activities'))),
      fluidRow(column(12, align = 'center', h4('Click below to get started'))),
      fluidRow(
        column(12, align = 'center',
               a(
                 img(src = 'btn_strava_connectwith_light.png'),
                 href = url
               )
        ))
    )
  })
  
  # See whether there is an authorisation code
  paramss <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(paramss)) {
    return()
  } else {
    load('token.RData')
  }

  # Reactive values for holding data
  session_data <- reactiveValues()
  session_data$token <- token
  
  # Get athlete data
  withProgress({
    shiny::setProgress(value=0.05,message = 'Logged in')
    
    # Retrieve the athlete
    message('Getting athlete...')
    my_athlete <- get_athlete(stoken = token)
    id <- my_athlete$id
    session_data$id <- id
    message('---Got athlete: ', my_athlete$username, ': ', my_athlete$firstname, ' ', my_athlete$lastname,
            ' (id: ', id, ')')
    
    # Save token
    cache_dir <- paste0('./cache/', id)
    dir.create(cache_dir,showWarnings = FALSE)
    saveRDS(token, paste0(cache_dir, '/token.rds'))
    
    # Save lastname too
    file.create(paste0(cache_dir, '/', my_athlete$firstname, ' ', my_athlete$lastname), showWarnings = FALSE)
    
    shiny::setProgress(value=0.5,message = 'Done getting athlete.')
    shiny::setProgress(value=0.65,message = 'Downloading activity data')
    message('Just fetching some data')
    my_acts <- get_activity_list_by_page(token,10,1)

    shiny::setProgress(value=0.35,message = 'Woohoo! Done downloading activity data. Now processing it.')
    
    my_acts.df <- my_acts %>% 
      tidy_activities()
    
    # Upodate the in-session data specific to this athlete
    session_data$athlete <- my_athlete
    session_data$activities <- my_acts.df
    
    shiny::setProgress(value=1,message = 'Complete')
    
  },
  min = 0,
  max = 1)

  
  output$development_text <- renderText({
    sda <- session_data$athlete
    fn <- sda$firstname
    paste0('Hi ', fn, '. This app is still under development. Come back soon!')
  })
  
  
  onSessionEnded(function() {
    message('---Disconnecting from database')
    dbDisconnect(con)
    message('---Deleting token')
    file.remove('token.RData')
    
    #   ## TO DO: GET STREAMS UPON SESSION END
    #   message('Fetching streams')
    #   # Get old streams
    #   message('---Reading in old streams for this athlete')
    #   isolate(old_streams <- dbReadTable(conn = con, name = 'streams'))
    #   message('---Disconnecting from database')
    #   print(head(old_streams))
    #   dbDisconnect(con)
    #   if(nrow(old_streams) == 0){
    #     message('------No old streams for this athlete.')
    #   } else {
    #     message('------', nrow(old_streams), ' rows of streams already in db for this athlete')
    #   }
    # 
    #   # Get new streams
    #   message('---Reading in new streams from API')
    #   ss <- reactive(session_data)
    #   save(ss, file = 'x.RData')
    #   ssl <- reactiveValuesToList(ss)
    #   save(ssl, file = 'xl.RData')
    #   streams <- get_streams(token = ss$token,
    #                          activities = ss$activities)
    #   add_streams <- streams %>% filter(!id %in% old_streams$id)
    #   if(nrow(add_streams) == 0){
    #     message('------No new streams to be added for this athlete. DB not modified.')
    #   } else {
    #     message('------Reconnecting to database because ', nrow(add_streams), ' rows of new streams were found. Adding to database...')
    #     # Reconnect to the database
    #     con = do.call(DBI::dbConnect, creds_list)
    #     dbWriteTable(con, "streams", data.frame(add_streams), append = TRUE, row.names = FALSE)
    #     message('Disconnecting from database')
    # 
    #   }
  })
}


onStop(function() {
  message('---Disconnecting from database')
  dbDisconnect(con)
  message('---Deleting token')
  file.remove('token.RData')
  
})

shinyApp(uiFunc, server)
