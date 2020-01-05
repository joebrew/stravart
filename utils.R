parse_authorisation_code <- function(session) {
  pars <- parseQueryString(session$clientData$url_search)
  return(pars$code)
}

load_application_config <- function() {
  
  filename <- 'config.yml'
  
  if (!file.exists(filename)) stop('No config.yml file found')
  
  # read credentials
  config <- yaml::read_yaml(filename)
  
  # load as environment variables
  config %>% 
    walk2(
      names(config),
      function(value,key) {
        value <- list(value)
        names(value) <- key
        do.call(Sys.setenv, value)
      }
    )
  
}

parse_application_url <- function(session) {
  paste0(session$clientData$url_protocol,
                    "//",
                    session$clientData$url_hostname, 
                    ifelse(
                      session$clientData$url_hostname == "127.0.0.1", 
                      ":",
                      session$clientData$url_pathname
                    ),
                    session$clientData$url_port
  )
}

logerror_stop <- function(msg,logger) {
  logerror(msg,logger=logger)
  stop(msg)
}

validate_credentials <- function(authorisation_code) {
  if (nchar(Sys.getenv('strava_app_client_id')) == 0) logerror_stop('strava_app_client_id is blank',logger='authentication')
  if (nchar(Sys.getenv('strava_app_secret')) == 0) logerror_stop('strava_app_secret is blank',logger='authentication')
  
  loginfo(glue('Using client id: {Sys.getenv(\'strava_app_client_id\')}'),
          logger = 'authentication')
  loginfo(glue('Using secret: {Sys.getenv(\'strava_app_secret\')}'),
          logger =
            'authentication')
  loginfo(glue('Using auth code: {authorisation_code}'),
          logger = 'authentication')
}

post_authorisation_code <- function(
  authorisation_code,
  strava_app_client_id,
  strava_app_secret
) {
    
  # post authorisation code
  response <- POST(url = 'https://www.strava.com/oauth/token',
                   body = list(
                     client_id = strava_app_client_id,
                     client_secret = strava_app_secret,
                     code = authorisation_code,
                     scope = "activity:read"
                   )
  )
  # returns json/list containing basic athlete information including the access token
  return(content(response))
}

#' Tidy activity list into a nested tibble
#'
#' @param .data a list
#'
#' @return a tibble
#' @export
#'
tidy_activities <- function(.data) {
  
  # compile list into data frame
  .data <- .data %>% 
    compile_activities() %>% 
    as.tibble
  
  # cast dates
  date_cols <- str_subset(colnames(.data),'date')
  .data[date_cols] <-  .data[date_cols] %>% 
    map(~strptime(.x,'%Y-%m-%dT%H:%M:%SZ')) %>% 
    map(as.POSIXct)
  
  # create date strings
  .data <- .data %>% 
    mutate(start_date_string=strftime(start_date,'%Y-%m-%d'))
  
  # add week and month ends + week and month starts
  .data <- .data %>% 
    mutate(week_end=as.Date(ceiling_date(start_date %>% as.Date,'week',week_start = 1))) %>% 
    mutate(month_end=as.Date(ceiling_date(start_date %>% as.Date,'month')-1)) %>% 
    mutate(week_start=as.Date(floor_date(start_date %>% as.Date,unit='week',week_start = 1))+1) %>% 
    mutate(month_start=start_date %>% as.Date %>% floor_date(unit = 'month') %>% as.Date())
    
  
  # create title: date + name combination
  .data <- .data %>% 
    mutate(title = paste(start_date_string,' - ',name,sep=''))
  
  # calculate route coordinates from polyline
  # .data <- .data %>% 
  #   mutate(route = map.summary_polyline %>% 
  #            map(function(polyline) {
  #              if (!is.na(polyline)) {
  #                # polyline %>%  decode_Polyline() %>% 
  #                #   separate(latlon,into=c('lat','lon'),sep = ',') %>% 
  #                #   # cast to numeric
  #                #   mutate(lat=as.numeric(lat)) %>% 
  #                #   mutate(lon=as.numeric(lon))
  #                polyline %>% decode %>% as.data.frame
  #              } else {
  #                NA
  #              }
  #            })
    # )
  
  # make numeric
  if('average_heartrate' %in% names(.data))
    .data <- .data %>% 
      mutate_at(vars(average_heartrate),as.numeric)
  
  # # replace start coordinates with higher precision coords from decoded polyline
  # .data <- .data %>% 
  #   mutate(start_latitude=route %>% map('lat') %>% map(1) %>% map_dbl(~ ifelse(is.null(.x),NA,.x) )) %>% 
  #   mutate(start_longitude=route %>% map('lon') %>% map(1) %>% map_dbl(~ ifelse(is.null(.x),NA,.x) ))
  
  return(.data)
}

#' Filter a list of activities by distance radius
#'
#' Filter activities by distance from particular starting lat/lon
#'
#' @param lon start longitude
#' @param lat start latitude
#' @param radius radius in meters
#'
#' @return
#' @export
#'
filter_within_radius <- function(data,lon,lat,radius) {
  if (!require(geosphere)) library(geosphere)
  
  # validate
  if (!all(c('start_latitude','start_longitude') %in% colnames(data))) stop('data must contain columns start_latitude and start_longitude')
  
  # calculate distance matrix
  distance_matrix <- distm(
    data %>% select(start_longitude,start_latitude),
    c(lon,lat)
  ) %>% 
    as.tibble %>% 
    mutate(id=data$id) %>% 
    select(id,everything()) %>% 
    set_names(c('id','distance')) %>% 
    filter(distance<=radius)
  
  # filter existing
  data <- data %>% 
    filter(id %in% distance_matrix$id)
  
  return(data)
}


#' Render leaflet map
#'
#' @param data a actframe object
#' @param urlTemplate url template from which to fetch map tiles
#' @param opacity line opacity
#' @param weight line weight
#' @param colour line colour
#'
#' @return
#' @export
#'
get_leaflet_heat_map <- function(
  data,
  urlTemplate = "http://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}.png",
  opacity = 0.01,
  weight = 3,
  colour = 'blue',
  markers=F
) {
  # extract non NA routes
  routes <- data %>% 
    select(id,title,route) %>% 
    filter(!is.na(route)) %>% 
    unnest()
  
  routes <- routes %>% split(.$id)
  
  # Plot map
  map <- leaflet() %>% 
    addTiles(urlTemplate = urlTemplate)
  
  for (r in routes) {
    map <- map %>% addPolylines(
      lng = r$lon,
      lat = r$lat,
      layerId = r$id,
      color=colour,
      opacity = opacity,
      weight = weight
    )
    
    if (markers) {
      map <- map %>%
        addMarkers(
          lng = r$lon[1],
          lat = r$lat[1],
          layerId = r$id[1],
          popup = r$title
        )
    }
    
  }
  map
}

get_activity_list_by_page <- function(stoken,per_page=200,pages=1) {
  get_pages(url_ = url_activities(),
            stoken=stoken,
            per_page=per_page,
            page_max=pages
  )
}


# Function for cleaning up athlete
tidy_athlete <- function(ath){
  cols <- c('id', 'username', 'firstname', 'lastname',
            'city', 'state', 'country', 'sex')
  for(i in 1:length(cols)){
    this_col <- cols[i]
    this_obj <- ath[[this_col]]
    if(is.null(this_obj)){
      ath[[this_col]] <- NA
    }
  }
  
  out <- tibble(id = ath$id,
                username = ath$username,
                firstname = ath$firstname,
                lastname = ath$lastname,
                city = ath$city,
                state = ath$state,
                country = ath$country,
                sex = ath$sex)
  return(out)
}

# Function for extracting starting locations
extract_starting_locations <- function(activities){
  activities %>% dplyr::distinct(start_latlng1, start_latlng2) %>%
    dplyr::rename(start_latitude = start_latlng1,
                  start_longitude = start_latlng2) %>%
    filter(!is.na(start_longitude),
           !is.na(start_latitude)) %>%
    mutate(starting_location_id = paste0(
      start_longitude, ',',
      start_latitude
    ))
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

theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

# Function for generating the starting location id in an activities df
generate_starting_location_id <- function(activities){
  x <- activities %>%
    mutate(starting_location_id = ifelse(is.na(start_longitude) | is.na(start_latitude),
                                         NA,
                                         paste0(start_longitude,',',start_latitude)))
  x$starting_location_id
}

# Reverse geocode function

reverse_geocode <- function(slids){
  # slids should be a pasted together character vector of lng/lat, separated by a comma, rounded to two digits
  # for example: "2.19,41.58"
  # First, separate into lng, lat df
  slids_split <- strsplit(slids, ',')
  lng <- lapply(slids_split, function(x){x[1]}) %>% unlist
  lat <- lapply(slids_split, function(x){x[2]}) %>% unlist
  df <- tibble(start_longitude = lng,
               start_latitude = lat,
               starting_location_id = slids) 
  starting_locations <- df %>% dplyr::distinct(start_longitude,
                               start_latitude,
                               starting_location_id,
                               .keep_all = TRUE)
  if(nrow(starting_locations) > 0){
    starting_locations$city <- starting_locations$zip <- starting_locations$state <- starting_locations$country <- as.character(NA)
    # Doing in a loop, even though not necessary, so as to save in case of failure
    for(i in 1:nrow(starting_locations)){
      message('Geocoding ', i, ' of ', nrow(starting_locations))
      this_activity <- starting_locations[i,]
      if(!is.na(this_activity$start_latitude)){
        x <- revgeo(longitude=this_activity$start_longitude, latitude=this_activity$start_latitude, provider = 'photon', output="frame")
        if('city' %in% names(x)){
          starting_locations$city[i] <- as.character(x$city)
        }
        if('zip' %in% names(x)){
          starting_locations$zip[i] <- as.character(x$zip) 
        }
        if('state' %in% names(x)){
          starting_locations$state[i] <- as.character(x$state)
        }
        if('country' %in% names(x)){
          starting_locations$country[i] <- as.character(x$country)
        }
        Sys.sleep(0.1)
      }
    }
  }
  return(starting_locations)
}

# Decode polylines into a lng/lat df
decode_df <- function(activities){
  decoded <- googlePolylines::decode(activities$map.summary_polyline)
  out_list <- list()
  for(i in 1:nrow(activities)){
    out <- tibble(id = activities$id[i],
                  lon = decoded[[i]]$lon,
                  lat = decoded[[i]]$lat)
    out_list[[i]] <- out
  }
  out <- bind_rows(out_list)
  return(out)
}

# Get average location for each activity
get_avg_location <- function(decoded_df){
  out <- decoded_df %>%
    group_by(id) %>%
    summarise(lon = mean(lon),
              lat = mean(lat))
  return(out)
}
