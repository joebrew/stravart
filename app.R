library(shiny)
source('global.R')

# Read in credentials
creds <- yaml::yaml.load_file('credentials.yaml')

if(interactive()){
  message('Interactive/testing mode')
  # testing url only
  options(shiny.port = 8100)
  app_url <- creds$url_local
} else {
  app_url <- creds$url_remote
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

ui <- fluidPage(
  # Regular UI goes here
  verbatimTextOutput("code"),
  mobileDetect('isMobile'),
  textOutput('isItMobile')
)
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    # OAuthorize
    # url <- oauth2.0_authorize_url(api, app, scope=scope, query_extra=params)
    # redirect <- sprintf("location.replace(\"%s\");", url)
    uiOutput('auth_submit')
    # tags$script(HTML(redirect))
  } else {
    # Manually create a token
    token <- oauth2.0_token(
      endpoint = api,
      app = app,
      credentials = oauth2.0_access_token(api, app, parseQueryString(req$QUERY_STRING)$code, user_params=params),
      cache = TRUE
    )
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
      fluidRow(
        column(12, align = 'center',
               a(
                 img(src = 'btn_strava_connectwith_light.png'),
                 href = url
               )
        )
      )
    )
  })
  
  # See whether there is an authorisation code
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
    # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  
  # Reactive values for holding data
  session_data <- reactiveValues()
  
  # Get athlete data
  withProgress({
    shiny::setProgress(value=0.05,message = 'Logged in')
    
    # Retrieve the athlete
    message('Getting athlete...')
    my_athlete <- get_athlete(stoken = token)
    id <- my_athlete$id
    message('---Got athlete: ', my_athlete$username, ': ', my_athlete$firstname, ' ', my_athlete$lastname,
            ' (id: ', id, ')')
    
    # Save token
    cache_dir <- paste0('./cache/', id)
    dir.create(cache_dir,showWarnings = FALSE)
    saveRDS(token, paste0(cache_dir, '/token.rds'))
    
    shiny::setProgress(value=0.2,message = 'Connecting to Strava data')
    
    # Fetch activities, but first see if that athlete already has some activities in the database
    # (fetch fewer if the athlete already has some)
    ids_already_in_db <- 
      dbGetQuery(conn = con,
                 statement = paste0('select id from activities where activities."athlete.id" = ',
                                    "'",
                                    id,
                                    "'")) %>% .$id
    athlete_ids_already_in_db <- 
      dbGetQuery(conn = con,
                 statement = paste0('select id from athletes')) %>% .$id
    activities_column_names <- 
      names(dbGetQuery(conn = con,
                       statement = paste0('select * from activities limit 1'))[0,])
    # Get fewer activities if the id is already in the db
    # (this is just for the sake of speed, and should be improved at some point so as to ensure we're not over/under-fetching)
    shiny::setProgress(value=0.25,message = 'Downloading activity data')
    if(id %in% athlete_ids_already_in_db){
      message('Athlete already in DB. Just fetching some data')
      my_acts <- get_activity_list_by_page(token,30,1)
    } else {
      message('New athlete. Fetching lots of data')
      my_acts <- get_activity_list_by_page(token,200,100)
    }
    shiny::setProgress(value=0.35,message = 'Done downloading activity data')
    
    my_acts.df <- my_acts %>% 
      tidy_activities()
    # Remove those that are already in the database
    new_acts <- my_acts.df %>%
      filter(!id %in% ids_already_in_db)
    # Add new_acts to db if there are any
    if(nrow(new_acts) > 0){
      # Remove excess columns
      new_acts <- data.frame(new_acts)
      new_acts <- new_acts[,names(new_acts) %in%activities_column_names]
      message('---Writing ', nrow(new_acts), ' new activities to the database for athlete ', id)
      # Write the new acts to the table
      dbWriteTable(conn = con,
                   name = 'activities',
                   value = new_acts,
                   append = TRUE,
                   row.names = FALSE)
    } else {
      message('---No new activities for athlete ', id)
    }
    # Retrieve the full activities for this athlete from the table
    activities_in_db <- 
      dbGetQuery(conn = con,
                 statement = paste0('select * from activities where activities."athlete.id" = ',
                                    "'",
                                    id,
                                    "'"))
    
    # Athlete info
    # If this athlete is already in the db, drop the  and overwrite
    if(id %in% athlete_ids_already_in_db){
      # Delete
      dbSendQuery(conn = con,
                  statement = paste0('DELETE FROM athletes WHERE id = ', id))
      # Add new row
      dbWriteTable(conn = con,
                   name = 'athletes', 
                   value = data.frame(tidy_athlete(my_athlete)),
                   append = TRUE,
                   row.names = FALSE)
      
    }

    # GEOCODING
    shiny::setProgress(value=0.6,message = 'Extracting starting locations for reverse geocoding')
    # Get starting location ids
    slids <- generate_starting_location_id(activities_in_db)
    slids <- slids[!is.na(slids)]
    slids <- sort(unique(slids))
    # Get already geocoded starting locations
    old_starting_locations <- dbReadTable(conn = con,name = 'starting_locations')
    # Remove those slids which are already geocoded
    slids <- slids[!slids %in% old_starting_locations$starting_location_id]
    shiny::setProgress(value=0.7,message = 'Reverse geocoding activity starting locations')
    if(length(slids) > 0){
      # Geocode them
      geocoded_slids <- reverse_geocode(slids = slids)
      # Add the geocoded slids to the db
      message('Adding newly geocoded starting locations to the database')
      dbWriteTable(con, "starting_locations", data.frame(geocoded_slids), append = TRUE, row.names = FALSE)
    }
    
    # Upodate the in-session data specific to this athlete
    session_data$athlete <- my_athlete
    session_data$activities <- activities_in_db
    session_data$starting_locations <- dbReadTable(conn = con, 
                                                   name = 'starting_locations')
    
    shiny::setProgress(value=1,message = 'Complete')
    
  },
  min = 0,
  max = 1)

  output$code <- renderText({
    sda <- session_data$athlete
    sda$lastname
  })
}
onStop(function() {
  dbDisconnect(con)
})
shinyApp(uiFunc, server)