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

# app_url <- creds$url_remote

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
                # sidebarLayout(
                #   sidebarPanel(h2('Side stuff')),
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                # tabPanel("Plot", 
                                #          fluidPage(
                                #            
                                #            leafletOutput("leaf")
                                #            
                                #            
                                #          )),
                                tabPanel("Chart", 
                                         fluidPage(
                                           fluidRow(column(12, align = 'center', h3(textOutput('development_text')))),
                                           mobileDetect('isMobile'),
                                           textOutput('isItMobile'),
                                           fluidRow(column(12,align = 'center',
                                                           plotOutput('plot1'))),
                                           fluidRow(column(6,align = 'center',
                                                           plotOutput('plot2'))))))))
                # )


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
        )
      ),
      fluidRow(
        column(12, align="center",
               div(style="display: inline-block;",img(src="img/a.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/b.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/d.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/p.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/o.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/n.png", height=300, width=300)))),
      fluidRow(
        column(12, align="center",
               div(style="display: inline-block;",img(src="img/j.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/g.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/h.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/r.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/l.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/i.png", height=300, width=300)))),
      fluidRow(
        column(12, align="center",
               div(style="display: inline-block;",img(src="img/c.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/e.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/f.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/m.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/q.png", height=300, width=300)),
               div(style="display: inline-block;",img(src="img/k.png", height=300, width=300))))
    )
  })
  
  # See whether there is an authorisation code
  paramss <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(paramss)) {
    return()
  } else {
    message('paramss is')
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
    
    shiny::setProgress(value=0.2,message = 'Connecting to Strava to retrieve your data. Be patient. This will take a long time.')
    
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
    shiny::setProgress(value=0.25,message = 'Keep being patient: Downloading activity data')
    if(id %in% athlete_ids_already_in_db){
      already <- TRUE
      message('Athlete already in DB. Just fetching some data')
      my_acts <- get_activity_list_by_page(token,30,1)
    } else {
      already <- FALSE
      message('New athlete. Fetching lots of data')
      my_acts <- get_activity_list_by_page(token,200,100)
    }
    shiny::setProgress(value=0.35,message = 'Woohoo! Done downloading activity data. Now processing it.')
    
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
    shiny::setProgress(value=0.7,message = 'This could take a while. Reverse geocoding activity starting locations')
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

  
  output$development_text <- renderText({
    sda <- session_data$athlete
    fn <- sda$firstname
    paste0('Hi ', fn, '. This app is still under development. But Joe now has your data. Thanks. Come back soon!')
  })
  
  output$plot1 <- renderPlot({
    activities <- session_data$activities
    if(!is.null(activities)){
      make_tiny_maps(activities = activities)
      }
  })
  
  output$plot2 <- renderPlot({
    activities <- session_data$activities
    starting_locations <- session_data$starting_locations
    
    if(!is.null(activities)){
      make_location_maps(activities = activities,
                         starting_locations = starting_locations)  
    }
  })
  
  
  output$leaf <- renderLeaflet({
    m <- leaflet() %>% 
      addTiles() %>% 
      addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                     circleMarkerOptions = F, polygonOptions = F)
  })
  
  observeEvent(input$leaf_draw_new_feature, {
    feat <- input$leaf_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
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
