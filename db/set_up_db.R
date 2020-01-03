
require(dplyr)
require(RPostgreSQL)
require(readr)
require(DBI)
library(yaml)
source('../global.R', chdir = T)
library(revgeo)

# Get creds
creds <- yaml.load_file('../credentials.yaml')

# Define the token
stoken <- httr::config(token = strava_oauth(app_name = 'GPSart', 
                                            app_client_id = 19335, 
                                            app_secret = creds$app_secret, 
                                            app_scope="activity:read"))

# Connect to the db
pg = DBI::dbDriver("PostgreSQL")
con = DBI::dbConnect(pg, dbname="stravart")

# Get athlete
my_athlete <- get_athlete(stoken = stoken)
athlete <- tidy_athlete(my_athlete)
message('Adding athlete to the database')
dbWriteTable(con, "athletes", data.frame(athlete), append = TRUE, row.names = FALSE)


unstack(stack(my_athlete))
cols <- names(my_athlete)
vals <- unlist(my_athlete)
names(vals) <- cols

# Get some activities
my_acts <- get_activity_list_by_page(stoken,200,100)
my_acts_compiled <- compile_activities(actlist = my_acts)
# # Clean them up
# activities <- my_acts %>% 
#   tidy_activities()
activities <- my_acts_compiled
# Write them to the database
message('Adding activities to the database')
dbWriteTable(con, "activities", data.frame(activities), append = TRUE, row.names = FALSE)


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
dbWriteTable(con, "streams", data.frame(streams), append = TRUE, row.names = FALSE)




# Reverse geocode the starting locations

# Get and reverse geocode the starting locations
# library(ggmap)
# register_google(key = Sys.getenv('google_key'))
starting_locations <- activities %>% dplyr::distinct(start_latlng1, start_latlng2) %>%
  dplyr::rename(start_latitude = start_latlng1,
                start_longitude = start_latlng2) %>%
  filter(!is.na(start_longitude),
         !is.na(start_latitude))

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
dbWriteTable(con, "starting_locations", starting_locations, append = TRUE, row.names = FALSE)


# disconnect from the database
dbDisconnect(con) 

