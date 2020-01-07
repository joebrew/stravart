library(shiny)
library(httpuv)
source('global.R')
source('plot_functions.R')
source('utils.R')
oob <- FALSE
load_application_config()
if(interactive()){
  message('Interactive/testing mode')
  # testing url only
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100/"
} else {
  APP_URL <- "http://bohemia.team/stravart"
}
redirect_uri <- APP_URL
app <- oauth_app(appname = 'GPSArt', 
                        key = Sys.getenv('strava_app_client_id'), 
                        secret = Sys.getenv('strava_app_secret'),
                 redirect_uri = APP_URL)
api <- oauth_endpoint(request = "https://www.strava.com/oauth/authorize?", 
                             authorize = "https://www.strava.com/oauth/authorize", 
                             access = "https://www.strava.com/oauth/token")

scope <- "activity:read_all"
params <- list(client_id='19335',
               response_type='code',
               redirect_uri= APP_URL,
               # scope = "activity:read_all"
               approval_prompt='auto')
has_auth_code <- function(params) return(!is.null(params$code))

ui <- fluidPage(
  # Your regular UI goes here, for when everything is properly auth'd
  verbatimTextOutput("code")
)


# Main ----
uiFunc <- function(req) {
  # if (interactive()) {
  #   # Skip OAuth flow
  #   return(ui)
  # }
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
    # Get user details
    # user <- GET("https://openidconnect.googleapis.com/v1/userinfo", config(token=token))
    # stop_for_status(user)
    # user <<- content(user, as="parsed")
    return(ui)
  }
}

server <- function(input, output, session) {
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  save(app, api, params, file = 'x.RData')
  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  
  # resp <- GET("https://api.github.com/user", config(token = token))
  resp <- get_athlete(stoken = token)
  # TODO: check for success/failure here
  
  output$code <- renderText({
    resp$lastname
  })
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)