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
    uiOutput('auth_submit_button')
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
  output$auth_submit_button <- renderUI({
    url <- oauth2.0_authorize_url(api, app, scope=scope, query_extra=params)
    a(
      img(src = 'btn_strava_connectwith_light.png'),
      href = url
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
  
  resp <- get_athlete(stoken = token)

  output$code <- renderText({
    resp$lastname
  })
}
onStop(function() {
  dbDisconnect(con)
})
shinyApp(uiFunc, server)