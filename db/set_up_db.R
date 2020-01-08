
require(dplyr)
require(RPostgreSQL)
require(readr)
require(DBI)
library(yaml)
source('../global.R', chdir = T)
source('../utils.R')
# Get creds
creds <- yaml.load_file('../credentials.yaml')
creds_list <- credentials_extract( credentials_path = '../')
creds_list <- creds_list[names(creds_list) %in% c('dbname', 'host', 'port', 'user', 'password')]
creds_list$drv <- DBI::dbDriver("PostgreSQL")

# Define the token

# # Specific id
watt <- where_are_the_tokens()
id = 21309810
token <- readRDS(paste0('../cache/', id, '/token.rds'))
# # # Anyone
# stoken <- httr::config(token = strava_oauth(app_name = 'GPSart',
#                                             app_client_id = 19335,
#                                             app_secret = creds$app_secret,
#                                             app_scope="activity:read"))
my_athlete <- get_athlete(stoken = token)
paste0(my_athlete$firstname, ' ', my_athlete$lastname)
id <- my_athlete$id

dir.create(paste0('../cache/', id), showWarnings = FALSE)
saveRDS(stoken, file = paste0('../cache/', id, '/token.rds'))

# Connect to the db
con = do.call(DBI::dbConnect, creds_list)

# Read in current stuff in the db
old_starting_locations <- dbReadTable(conn = con,name = 'starting_locations')
old_athletes <- dbReadTable(conn = con, name = 'athletes')
old_activities <- dbReadTable(conn = con, name = 'activities')
old_streams <- dbReadTable(conn = con, name = 'streams')

# Get athlete
# my_athlete <- get_athlete(stoken = stoken)
athlete <- tidy_athlete(my_athlete)
message('Adding athlete to the database')
add_athletes <- athlete %>% dplyr::filter(!id %in% old_athletes$id)
dbWriteTable(con, "athletes", data.frame(add_athletes), append = TRUE, row.names = FALSE)


# Get some activities
my_acts <- get_activity_list_by_page(token,200,100)
my_acts_compiled <- compile_activities(actlist = my_acts)
# # Clean them up
# activities <- my_acts %>% 
#   tidy_activities()
activities <- my_acts_compiled
# Write them to the database
message('Adding activities to the database')
add_activities <- activities %>%
  filter(!id %in% old_activities$id)
add_activities <- add_activities[,names(add_activities) %in% names(old_activities)]
dbWriteTable(con, "activities", data.frame(add_activities), append = TRUE, row.names = FALSE)


# Get some streams
streams_list <- list()
counter <- 0
for(i in 1:nrow(activities)){
  message(i, ' of ', nrow(activities))
  this_activity <- activities[i,]
  this_id <- this_activity$id
  if(!is.na(this_activity$start_latlng1)){
    counter <- counter + 1
    this_stream <- get_activity_streams(act_data = activities[i,], stoken = stoken)
    streams_list[[counter]] <- this_stream
    Sys.sleep(1.2)
  }
}
streams <- bind_rows(streams_list)
add_streams <- streams %>% filter(!id %in% old_streams$id)
dbWriteTable(con, "streams", data.frame(add_streams), append = TRUE, row.names = FALSE)

# Reverse geocode the starting locations

# Get and reverse geocode the starting locations
# library(ggmap)
# register_google(key = Sys.getenv('google_key'))

old_starting_locations <- dbReadTable(conn = con,name = 'starting_locations')
# old_starting_locations$starting_location_id <-
#   paste0(old_starting_locations$start_longitude,',',
#          old_starting_locations$start_latitude)

# Get the new starting locations
starting_locations <- generate_starting_location_id(activities)
starting_locations <- unique(sort(starting_locations))
starting_locations <- starting_locations[!is.na(starting_locations)]
need_to_geocode <- !starting_locations %in% old_starting_locations$starting_location_id
starting_locations <- starting_locations[need_to_geocode]
starting_locations <- data.frame(starting_location_id = starting_locations)
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
  
  message('Adding starting_locations to the database')
  dbWriteTable(con, "starting_locations", data.frame(starting_locations), append = TRUE, row.names = FALSE)
  
}


# disconnect from the database
dbDisconnect(con) 

db_streams = dbReadTable(conn = con, name = 'streams')
db_activities = dbReadTable(conn = con, name = 'activities')
db_athletes = dbReadTable(conn = con, name = 'athletes')
db_starting_locations = dbReadTable(conn = con, name = 'starting_locations')

x = db_streams %>% left_join(db_activities %>%
                               dplyr::select(athlete.id, start_latlng1,
                                             start_latlng2,
                                             id),
                             by = 'id') %>%
  left_join(db_athletes %>% mutate(id = as.character(id)),
            by = c('athlete.id' = 'id'))
sl <- extract_starting_locations(x)
x <- x %>% left_join(sl,
                     by = c('start_latlng1' = 'start_latitude',
                            'start_latlng2' = 'start_longitude'))
