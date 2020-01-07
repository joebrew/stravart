library(shiny)
source('global.R')
source('plot_functions.R')
source('utils.R')

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
api <- oauth_endpoint(request = "https://www.strava.com/oauth/authorize?", 
                      authorize = "https://www.strava.com/oauth/authorize", 
                      access = "https://www.strava.com/oauth/token")

scope <- "activity:read_all"
params <- list(client_id='19335',
               response_type='code',
               redirect_uri= app_url,
               # scope = "activity:read_all"
               approval_prompt='auto')

ui <- fluidPage(
  # Regular UI goes here
  verbatimTextOutput("code")
)
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    # OAuthorize
    url <- oauth2.0_authorize_url(api, app, scope=scope, query_extra=params)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
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
  
  resp <- get_athlete(stoken = token)

  output$code <- renderText({
    resp$lastname
  })
}

shinyApp(uiFunc, server)