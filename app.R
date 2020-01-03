library(shiny)

source('global.R')

if(interactive()){
  message('Interactive/testing mode')
  # testing url only
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://servername/path-to-app"
}

# Define function for authentication (modification of strava_oauth)
stravauth <- function (app_name, app_client_id, app_secret, app_scope = "public", 
                       cache = FALSE, redirect_uri = oauth_callback()){
  strava_app <- oauth_app(appname = app_name, key = app_client_id, 
                          secret = app_secret)
  strava_end <- oauth_endpoint(request = "https://www.strava.com/oauth/authorize?", 
                               authorize = "https://www.strava.com/oauth/authorize", 
                               access = "https://www.strava.com/oauth/token")
  oauth2.0_token(endpoint = strava_end, app = strava_app, scope = app_scope, 
                 cache = cache)
}

config_panel <- shinyjs::hidden(
  fluidRow(
    id='config_panel',
    box(
      width=12,
      status = 'primary',
      title = 'Set application Configuration',
      solidHeader = T,
      div(
        fluidRow(
          column(3,
                 textInput(
                   'input_strava_app_client_id',
                   "Client ID",
                   value = Sys.getenv('strava_app_client_id')
                 )
          ),
          column(
            3,
            textInput(
              'input_strava_app_secret',
              "Client Secret",
              value = Sys.getenv('strava_app_secret')
            )
          ),
          column(
            3,
            textInput(
              'input_strava_app_url',
              "Application URL",
              value = Sys.getenv('strava_app_url')
            )
          ),
          column(
            3,
            actionButton('input_save_config','Save')
            #uiOutput('auth_submit_button')
          ),
          br()
        )
      )
    )
  )
)

loggin_panel <- shinyjs::hidden(
  
  fluidRow(
    id='loggin_panel',
    box(
      width=12,
      status = 'primary',
      title = 'Click to login with Strava',
      solidHeader = T,
      div(
        uiOutput('auth_submit_button')
        
      )
    )
  )
)




ui_activity_filters <- div(
  

  # shiny::selectInput('selected_period',
  #                    label='Period',
  #                    choices = names(periods),
  #                    selected = 'Last 30 days',
  #                    multiple = F
  # ),
  # conditionalPanel(
  #   condition="input.selected_period == 'Custom'",
  #   dateRangeInput(inputId = 'selected_dates',
  #                  label = 'Select date range'
  #   )
  # ),
  # shiny::selectInput(
  #   inputId = 'selected_types',
  #   label='Select types',
  #   choices='',
  #   multiple = T
  # ),
  div(
    img(src='1.2 strava api logos/powered by Strava/pwrdBy_strava_light/api_logo_pwrdBy_strava_stack_light.png',width=199*.6,height=86*.6),
    style='display: block;margin-left: auto;margin-right: auto;width: 50%;'
  )
)

ui_footer <- fluidRow(
  box(
    width=12,
    column(4,img(src='api_logo_pwrdBy_strava_stack_light.png',width=199*.6,height=86*.6)),
    column(8,textOutput('welcome_line'))
  )
)

# ui_summary_chart <- plotOutput('summary_chart')

ui <- shinyUI(
  dashboardPage(
    title = 'GPSart',
    
    # HEADER ----
    dashboardHeader(
      title = 'GPSart'
      #titleWidth = 350
    ),
    
    # SIDEBAR ----
    dashboardSidebar(
      uiOutput('ui_logged_in')#,
      # ui_activity_filters
      #width = 350
    ),
    
    
    dashboardBody(
      useShinyjs(),
      config_panel,
      loggin_panel,
      tabBox(
        width = 12,
        tabPanel(
          'Example',
          fluidPage(h3('Under construction'))
          # summaryDataUI('summary_data')
        ),
        tabPanel(
          'Detailed visualizations',
          fluidPage(h3('Under construction'))
          # activityMapUI('map')
        )
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  # modules ----
  # callModule(summaryData,"summary_data",activities=get_filtered_activities)
  # callModule(activityMap,"map",activities=get_filtered_activities)
  
  # initialise app parameters
  app_parameters <- reactiveValues()
  
  # Read from the database
  
  
  # APPLICATION STARTUP, STATUS AND UI ----
  observe({
    
    # 1. Manage Cache ----
    if (cache) {
      app_parameters$logged_in <- T
      app_parameters$config_loaded <- T
      app_parameters$stoken=readRDS('./cache/stoken.rds')
      app_parameters$token_data=readRDS('./cache/token_data.rds')
      app_parameters$activities=readRDS('./cache/activities.rds')
      app_parameters$activities=readRDS('./cache/athlete.rds')
      return()
    }
    
    # 2. Load config ----
    app_parameters$config_loaded <- 
      tryCatch({
        load_application_config()
        loginfo('config.yml loaded',logger = 'config')
        T
      },error=function(e) {
        loginfo('No config.yml file found',logger = 'config')
        return(F)
      })
    
    # 3. Authorisation and API calls ----
    authorisation_code <- parse_authorisation_code(session)
    # check if authorisation code is empty
    if (is.null(authorisation_code)) {
      app_parameters$logged_in <- F
    } else {
      
      withProgress({
        
        app_parameters$logged_in <- T
        app_parameters$authorisation_code <- authorisation_code
        
        shiny::setProgress(value=0,message = 'Logging in...')
        
        # check credentials 
        validate_credentials(authorisation_code)
        
        # 3.1 Get stoken ----
        app_parameters$token_data <- post_authorisation_code(
          authorisation_code = authorisation_code,
          strava_app_client_id = Sys.getenv('strava_app_client_id'),
          strava_app_secret = Sys.getenv('strava_app_secret')
        )
        
        # verify access_token is available
        if ('access_token' %in% names(app_parameters$token_data)) {
          loginfo(glue('Using access token: {app_parameters$token_data$access_token} '),logger='authentication')
        } else {
          logerror_stop('Authorisation error',logger='authentication')
        }
        shiny::setProgress(value=0.05,message = 'Logged in')
        # app_parameters$stoken <- add_headers(Authorization = paste0("Bearer ",app_parameters$token_data$access_token))
        
        app_parameters$stoken <- httr::config(token = stravauth(app_name = 'GPSart', 
                                                                   app_client_id = 19335, 
                                                                   app_secret = creds$app_secret, 
                                                                   app_scope="activity:read",
                                                                redirect_uri = APP_URL))
        
        saveRDS(app_parameters$token_data,'./cache/token_data.rds')
        saveRDS(app_parameters$stoken,'./cache/stoken.rds')
        
        # 3.2 Download activity list ----
        
        shiny::setProgress(value=0.2,message = 'Connecting to strava')
        
        loginfo('Downloading activities...',logger='api')
        shiny::setProgress(value=0.25,message = 'Downloading activity list')
        my_athlete <- get_athlete(stoken = app_parameters$stoken)
        if(my_athlete$id %in% athletes$id){
          message('ALREADY HAVE THIS ATHLETE. LOADING OLD DATA...')
        }
        my_acts <- get_activity_list_by_page(app_parameters$stoken,200,100)

        shiny::setProgress(value=0.6,message = 'Transforming data')
        
        loginfo(glue('Downloaded {length(my_acts)} activities'),logger='api')
        
        # process
        loginfo('Tidying activities',logger='api')
        my_acts.df <- my_acts %>% 
          tidy_activities()
        loginfo('Tidying complete',logger='api')
        
        
        app_parameters$activities <- my_acts.df
        app_parameters$athlete <- my_athlete
        
        shiny::setProgress(value=1,message = 'Complete')
      },
      min = 0,
      max = 1
      )
      
      # save to global parameters
      saveRDS(my_acts,'./cache/raw_activities.rds')  
      saveRDS(my_acts.df,'./cache/activities.rds')  
      saveRDS(my_athlete,'./cache/athlete.rds')  
      
      # cache
      dir.create('cache',showWarnings = F)
    }

    # show config panel
    if (!app_parameters$config_loaded) {
      shinyjs::show('config_panel')
    } else {
      shinyjs::hide('config_panel')
    }
    
    # show loggin panel
    if (app_parameters$config_loaded & !app_parameters$logged_in) {
      shinyjs::show('loggin_panel')
    } else {
      shinyjs::hide('loggin_panel')
    }
    
  })
  
  # GET STATES ----
  # 1. get config loaded state ----
  output$config_loaded <- reactive({
    value <- app_parameters$config_loaded
    app_parameters$config_loaded
  })
  outputOptions(output,'config_loaded', suspendWhenHidden = FALSE)
  
  # 2. get logged in state ----
  output$logged_in <- reactive({
    app_parameters$logged_in
  })
  outputOptions(output,'logged_in', suspendWhenHidden = FALSE)
  
  # DYNAMICALLY CAPTURE APPLICATION URL ----
  observe({
    app_url <- parse_application_url(session)
    loginfo(glue('Captured application url as {app_url}'),logger='config')
    updateTextInput(session,inputId = 'input_strava_app_url',value = app_url)
  })
  
  # SAVE CONFIG FROM FORM ----
  observeEvent(input$input_save_config,{
    # capture application credentials into list
    credentials <- list(
      strava_app_url = input$input_strava_app_url,
      strava_app_client_id  = as.numeric(input$input_strava_app_client_id),
      strava_app_secret = input$input_strava_app_secret,
      strava_app_authorisation_url = glue('https://www.strava.com/oauth/authorize?client_id={as.numeric(input$input_strava_app_client_id)}&response_type=code&redirect_uri={input$input_strava_app_url}&approval_prompt=auto&state=')
    )
    
    # write to yaml file
    yaml::write_yaml(credentials %>% map(as.character),'config.yml')
    
    # load into app
    load_application_config()
    
    # change app parameter
    app_parameters$config_loaded <- T
  })
  
  # output$auth_submit_button ----
  output$auth_submit_button <- renderUI({
    app_url <- parse_application_url(session)
    url <- glue('https://www.strava.com/oauth/authorize?client_id={Sys.getenv(\'strava_app_client_id\')}&response_type=code&redirect_uri={app_url}&approval_prompt=auto&state=')
    
    a(
      img(src = 'btn_strava_connectwith_light.png'),
      href = url
    )
  })
  
  # output$welcome_line ----
  # adds welcome line and triggers authentication to take place
  output$welcome_line <- renderText({
    stoken <- app_parameters$stoken
    token_data <- app_parameters$token_data
    glue('{token_data$athlete$firstname} {token_data$athlete$lastname}')
  })
  
  # Initialise UI ----
  # update form controls
  observeEvent(app_parameters$activities,{
    
    activities <- app_parameters$activities
    
    # 1. initialise dates ----
    daterange <- range(as.Date(activities$start_date))
    updateDateRangeInput(session=session,
                         inputId = 'selected_dates',
                         start = lubridate::floor_date(Sys.Date(),unit = 'year'),
                         end = strftime(Sys.Date(),'%Y-%m-%d'),
                         min = daterange[1],
                         max = strftime(Sys.Date(),'%Y-%m-%d')
    )
    # 2. initialise types ----
    types <- activities$type %>% unique %>% sort
    updateSelectInput(
      session = session,
      inputId = 'selected_types',
      choices = types,
      selected = types
    )
    
  })
  
  # # observe period ----
  # observeEvent(input$selected_period,{
  # 
  #   if(is.null(app_parameters$activities)) return()
  # 
  #   period <- input$selected_period
  # 
  #   dates <- periods[[period]]
  # 
  # 
  # 
  #   daterange <- range(as.Date(app_parameters$activities$start_date))
  # 
  #   dates[1] <- max(dates[1],daterange[1])
  #   dates[2] <- min(dates[2],daterange[2],Sys.Date())
  # 
  # 
  #   updateDateRangeInput(session=session,
  #                        inputId = 'selected_dates',
  #                        start = as.Date(dates[1]),
  #                        end = dates[2])
  # 
  # 
  # })
  
  #.---------------------- -----
  # ANALYTICS ----
  
  # get_filtered_activities ----
  get_filtered_activities <- reactive({
    
    shiny::validate(
      shiny::need(app_parameters$config_loaded==T,'Application config doesn\'t exist')
    )
    
    shiny::validate(
      shiny::need(app_parameters$logged_in==T,'You are not logged in')
    )
    
    #if (!app_parameters$logged_in) return()
    if (is.null(app_parameters$activities)) return()
    
    # # validate inputs are available
    # req(
    #   input$selected_dates,
    #   input$selected_types
    # )
    
    loginfo('Filter activities',logger='activities')
    
    filtered_activities <- activities
    # activities <- app_parameters$activities
    # 
    # date_range_filter <- input$selected_dates
    # types_filter <- input$selected_types
    
    
    # filtered_activities <- activities %>% 
    #   filter(
    #     start_date >= date_range_filter[1] & 
    #       start_date <= date_range_filter[2] + hms('23:59:59')
    #   ) %>% 
    #   filter(type %in% types_filter)
    
    shiny::validate(
      shiny::need(nrow(filtered_activities) > 0,message = 'No activities selected')  
    )
    
    return(filtered_activities)
  })
  
  output$ui_logged_in <- renderUI({
    li <- app_parameters$logged_in
    if(li){
      fn <- app_parameters$athlete$firstname
      ln <- app_parameters$athlete$lastname
      fluidPage(
        helpText(paste0('You are logged in as ',fn, ' ', ln))
      )
    } else {
      fluidPage(
        p('You are not logged in. Click the "Connect with Strava" button.')
      )
    }
  })
  
}

onStop(function() {
  dbDisconnect(con)
})
shinyApp(ui = ui, server = server, )