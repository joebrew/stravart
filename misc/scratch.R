
require(dplyr)
require(RPostgreSQL)
require(readr)
require(DBI)
library(yaml)
library(googlePolylines)
library(ggplot2)
library(gridExtra)

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


source('../global.R', chdir = T)


# Connect to the db
pg = DBI::dbDriver("PostgreSQL")
con = DBI::dbConnect(pg, dbname="stravart")

# Load activities
streams <- dbReadTable(conn = con, name = 'streams')
activities <- dbReadTable(conn = con,name = 'activities')
starting_locations <- dbReadTable(conn = con,name = 'starting_locations')
dbDisconnect(conn = con)

# Join activities to the starting locations
activities <- left_join(x = activities,
                        y = starting_locations,
                        by = c('start_latlng2' = 'start_longitude',
                               'start_latlng1' = 'start_latitude'))

# Unlist the activity polylines into a df
out <- list()
for(i in 1:nrow(activities)){
  message(i)
  this_activity <- activities[i,]
  these_lines <- googlePolylines::decode(this_activity$map.summary_polyline)
  these_lines <- data.frame(these_lines[[1]])
  these_lines$joiner <- 1
  this_activity$joiner <- 1
  these_lines <- left_join(these_lines, this_activity, by = 'joiner')
  out[[i]] <- these_lines
}

all_lines <- bind_rows(out)

# Plot everything into one faceted chart


# Make a plot, faceted by starting locations
dir.create('~/Desktop/cities')
cities <- sort(unique(all_lines$city[all_lines$type == 'Run']))
for(i in 1:length(cities)){
  col <- sample(rainbow(100), 1)
  this_city <- cities[i]
  message(this_city, ': ', i, ' of ', length(cities))
  pd <- streams %>% 
    dplyr::mutate(lon = lng) %>%
    left_join(activities %>% dplyr::select(id, start_date_local, type, city)) %>%
    filter(type == 'Run') %>%#,
           # start_date_local >= '2019-01-01',
           # start_date_local <= '2019-12-31') %>%
    filter(city == this_city)
  
  ggplot(data = pd,
         aes(x = lon,
             y = lat,
             group = id)) +
    geom_path(size = 0.25, lineend = "round",
              alpha = 0.3,
              color = col) +
    theme_black() +
    theme(
      # Specify panel options
      panel.spacing = ggplot2::unit(0, "lines"), 
      strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
      plot.margin = ggplot2::unit(rep(1, 4), "cm"),
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "black"),  
      plot.background = element_rect(color = "black", fill = "black")
    ) +
    theme(panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(),
          panel.background=element_blank()) + 
    theme(plot.background=element_rect(fill="black"),
          panel.background=element_rect(fill='black'), 
          legend.background= element_rect(fill="black", colour=NA),
          legend.key = element_rect(colour = NA, col = "black",
                                    size = .5, fill = 'black')) +
    theme(axis.line = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          # axis.title = element_blank(), 
          panel.border = element_blank(), 
          panel.spacing = unit(0, 
                               "lines"), 
          legend.justification = c(0, 0), 
          legend.position = c(0, 
                              0)) +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          plot.margin=unit(c(0,0,0,0), "lines")) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    # theme(legend.position = 'none') +
    theme(legend.position = 'bottomright') +
    labs(x = '', y = '') +
    coord_map()
  ggsave(paste0('~/Desktop/cities/', this_city, '.pdf'))
}

system('nautilus ~/Desktop/cities')

# library(strava) # https://github.com/marcusvolz/strava

pd = streams %>% 
  dplyr::mutate(lon = lng) %>%
  left_join(activities %>% dplyr::select(id, start_date_local, type)) #%>%
  # filter(type == 'Run',
  #        start_date_local >= '2019-01-01',
  #        start_date_local <= '2019-12-31')
data = pd
scales = 'free'
labels = FALSE

# Summarise data
summary <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(lon = mean(range(lon)),
                   lat = mean(range(lat)),
                   distance = max(distance))

# Decide if tracks will all be scaled to similar size ("free") or if
# track sizes reflect absolute distance in each dimension ("fixed")
if (scales == "fixed") {
  data <- data %>% dplyr::group_by(id) %>% # for each track,
    dplyr::mutate(lon = lon - mean(lon), # centre data on zero so facets can
                  lat = lat - mean(lat)) # be plotted on same distance scale
} else {
  scales = "free" # default, in case a non-valid option was specified
}

# Create plot
p <- ggplot2::ggplot() +
  ggplot2::geom_path(ggplot2::aes(lon, lat, group = id), data, size = 0.35, lineend = "round") +
  ggplot2::facet_wrap(~id, scales = scales) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(rep(1, 4), "cm"))

if (scales == "fixed") {
  p <- p + ggplot2::coord_fixed() # make aspect ratio == 1
}

# Add labels
if(labels) {
  p <- p +
    ggplot2::geom_text(ggplot2::aes(lon, lat, label = distance), data = summary,
                       alpha = 0.25, size = 3)
}

# Return plot
p
ggsave('~/Desktop/facets.pdf', height = 4, width = 7)

# Black background facets
pd = streams %>% 
  dplyr::mutate(lon = lng) %>%
  left_join(activities %>% dplyr::select(id, start_date_local, type)) #%>%
# filter(type == 'Run',
#        start_date_local >= '2019-01-01',
#        start_date_local <= '2019-12-31')
data = pd
scales = 'free'
labels = FALSE

# Summarise data
summary <- data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(lon = mean(range(lon)),
                   lat = mean(range(lat)),
                   distance = max(distance))

# Decide if tracks will all be scaled to similar size ("free") or if
# track sizes reflect absolute distance in each dimension ("fixed")
if (scales == "fixed") {
  data <- data %>% dplyr::group_by(id) %>% # for each track,
    dplyr::mutate(lon = lon - mean(lon), # centre data on zero so facets can
                  lat = lat - mean(lat)) # be plotted on same distance scale
} else {
  scales = "free" # default, in case a non-valid option was specified
}

# Create plot
p <- ggplot2::ggplot() +
  ggplot2::geom_path(ggplot2::aes(lon, lat, group = id), data, size = 0.15, lineend = "round",
                     col = 'darkred') +
  ggplot2::facet_wrap(~id, scales = scales) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(rep(1, 4), "cm"))

if (scales == "fixed") {
  p <- p + ggplot2::coord_fixed() # make aspect ratio == 1
}

# Add labels
if(labels) {
  p <- p +
    ggplot2::geom_text(ggplot2::aes(lon, lat, label = distance), data = summary,
                       alpha = 0.25, size = 3)
}

# Return plot
p +
  theme_black() +
  theme(
    # Specify panel options
    panel.spacing = ggplot2::unit(0, "lines"), 
    strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
    plot.margin = ggplot2::unit(rep(1, 4), "cm"),
    panel.background = element_rect(fill = "black", color  =  NA),  
    panel.border = element_rect(fill = NA, color = "black"),  
    plot.background = element_rect(color = "black", fill = "black")
  ) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        panel.background=element_blank()) + 
  theme(plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill='black'), 
        legend.background= element_rect(fill="black", colour=NA),
        legend.key = element_rect(colour = NA, col = "black",
                                  size = .5, fill = 'black')) +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        # axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.spacing = unit(0, 
                             "lines"), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 
                            0)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "lines")) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = '',
       y = '')
ggsave('~/Desktop/facetsdark.png', height = 4, width = 7)


# Elevation
pd <- streams %>%
  left_join(activities %>% dplyr::select(total_distance = distance,
                                         id)) %>%
  mutate(p = distance / total_distance) %>%
  group_by(id) %>%
  mutate(max_e = max(altitude)) %>%
  filter(max_e < 3500)
g <- ggplot(data = pd,
       aes(x = p,
           y = altitude,
           group = id)) +
  geom_area(alpha = 0.2) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(rep(1, 4), "cm"))
ggsave('~/Desktop/elevation.pdf', height = 4, width = 7)

# Github calendar
# devtools::install_github("marcusvolz/ggart")
# devtools::install_github("AtherEnergy/ggTimeSeries")
# install.packages("ggthemes")
# install.packages("viridis")
library(ggart)
library(ggthemes)
library(ggTimeSeries)
library(lubridate)
library(strava)
library(tidyverse)
library(viridis)
data = activities
summary <- data %>%
  mutate(time = as.POSIXct(start_date_local, format = '%Y-%m-%dT%H:%M:%SZ')) %>%
  mutate(year = strftime(time, format = "%Y"),
         date_without_month = strftime(time, format = "%j"),
         month = strftime(time, format = "%m"),
         day_of_month = strftime(time, format = "%d"),
         year_month = strftime(time, format = "%Y-%m")) %>%
  group_by(time, year, date_without_month, month, day_of_month, year_month) %>%
  summarise(total_dist = max(distance),
            moving_time = max(moving_time),
            total_time = max(elapsed_time)) %>%
  mutate(speed = (total_dist) / (moving_time /60^2)) %>%
  mutate(pace = (total_time / 60) / (total_dist)) %>%
  mutate(type = "day") %>%
  ungroup %>%
  mutate(id = as.numeric(row.names(.))) %>%
  filter(is.finite(pace))

time_min <- "2016-12-31"
time_max <- today()
max_dist <- 30

daily_data <- summary %>%
  group_by(time) %>%
  summarise(dist = sum(total_dist)) %>%
  ungroup() %>%
  mutate(time = as.Date(time)) %>%
  filter(complete.cases(.), time > time_min, time < time_max) %>%
  mutate(dist_scaled = ifelse(dist > max_dist, max_dist, dist))
p <- ggplot_calendar_heatmap(daily_data, "time", "dist_scaled",
                             dayBorderSize = 0.5, dayBorderColour = "white",
                             monthBorderSize = 0.75, monthBorderColour = "transparent",
                             monthBorderLineEnd = "round") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(name = "km", low = "#DAE580", high = "#236327", na.value = "#EFEDE0") +
  facet_wrap(~Year, ncol = 1) +
  theme_tufte() +
  theme(strip.text = element_text(), axis.ticks = element_blank(), legend.position = "bottom")

ggsave("calendar001.pdf", p, width = 30, height = 30, units = "cm", dpi = 300)

# Time of day
#install.packages("ggridges")
library(ggridges)
library(lubridate)
library(tidyverse)

compute_day_curve <- function(df_row) {
  start <- data[df_row, "start_time"]
  end <- data[df_row, "end_time"]
  wday <- as.character(data[df_row, "wday"])
  year <- as.character(data[df_row, "year"])
  result <- data.frame(time = seq(as.POSIXct("00:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("23:59:58", format = "%H:%M:%S"), by = 60))
    
  result$time_end <- lead(result$time,  default = as.POSIXct("23:59:59", format = "%H:%M:%S"))
  result$active <- ifelse(result$time > start$start_time & end$end_time < result$time_end, 1, 0)
  result$wday <- wday
  result$year <- year
  result
}

data_in <- streams %>%
  left_join(activities %>% dplyr::select(total_distance = distance,
                                         start_date_local,
                                         type,
                                         moving_time,
                                         id)) %>%
  mutate(p = distance / total_distance) %>%
  # filter(start_date_local >= '2018-01-01',
  #        start_date_local <= '2018-12-31') %>%
  filter(type %in% c('Hike', 'Run', 'Ride'))
data <- data_in %>%
  # Make all the same day
  mutate(start_time = paste0('2020-01-03T', unlist(lapply(strsplit(start_date_local, split = 'T'), function(x){x[2]})))) %>%
  mutate(start_time = as.POSIXct(start_time, format = '%Y-%m-%dT%H:%M:%SZ')) %>%
  mutate(end_time = as.POSIXct(start_date_local, format = '%Y-%m-%dT%H:%M:%SZ') + seconds(moving_time)) %>% 
  mutate(duration = moving_time) %>%
  group_by(id) %>%
  summarise(start_time = min(start_time), end_time = max(end_time),
            duration = max(duration),
            wday = wday(dplyr::first(start_date_local), week_start = 1),
            year = as.numeric(substr(dplyr::first(start_date_local), 1, 4))) %>%
  filter(year != 2020)







plot_data <- 1:nrow(data) %>%
  map_df(~compute_day_curve(.x), .id = "id") %>%
  filter(!is.na(active), active > 0) %>%
  mutate(wday = as.factor(wday))
# View(plot_data)
plot_data$wday <- factor(plot_data$wday, levels = rev(levels(plot_data$wday)))
p <- ggplot() +
  geom_density_ridges(aes(x = time, y = wday), plot_data, size = 0.5, fill = 'black', color = 'white') +
  theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0), labels = c("Sun", "Sat", "Fri", "Thu", "Wed", "Tue", "Mon")) +
  scale_x_datetime(expand = c(0, 0), date_labels = "%I:%M %p") +
  theme(panel.grid = element_blank(), plot.margin = unit(rep(1, 4), "cm")) +
  xlab(NULL) + ylab(NULL) +
  facet_wrap(~year, nrow = 1)
ggsave("ridges_inx.png", p, width = 35, height = 20, units = "cm")

data <- data_out %>%
  group_by(id) %>%
  summarise(start = min(time), end = max(time)) %>%
  mutate(start_time = as.POSIXct(strftime(start, format = "%H:%M:%S"), format = "%H:%M:%S"),
         end_time = as.POSIXct(strftime(end, format = "%H:%M:%S"), format = "%H:%M:%S"),
         duration = end_time - start_time,
         wday = wday(start, week_start = 1))

plot_data <- 1:nrow(data) %>%
  map_df(~compute_day_curve(.x), .id = "id") %>%
  filter(!is.na(active), active > 0) %>%
  mutate(wday = as.factor(wday))
View(plot_data)
plot_data$wday <- factor(plot_data$wday, levels = rev(levels(plot_data$wday)))
p <- ggplot() +
  geom_density_ridges(aes(x = time, y = wday), plot_data, size = 0.5) +
  theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0), labels = c("Sun", "Sat", "Fri", "Thu", "Wed", "Tue", "Mon")) +
  scale_x_datetime(expand = c(0, 0), date_labels = "%I:%M %p") +
  theme(panel.grid = element_blank(), plot.margin = unit(rep(1, 4), "cm")) +
  xlab(NULL) + ylab(NULL)
ggsave("ridges_out.png", p, width = 24, height = 20, units = "cm")

# Elevation facet
pd <- streams %>%
  left_join(activities %>% dplyr::select(total_distance = distance,
                                         start_date_local,
                                         type,
                                         id)) %>%
  mutate(p = distance / total_distance) %>%
  filter(start_date_local >= '2019-01-01',
         start_date_local <= '2019-12-31') %>%
  filter(type %in% c('Hike', 'Run', 'Ride')) %>%
  mutate(cumdist = distance) %>%
  group_by(id) %>%
  mutate(max_e = max(altitude)) %>%
  filter(max_e < 3500) %>%
  mutate(ele = altitude)
plot_elevations <- function(data, scale_free_y = F, nr  = NULL) {
  # Compute total distance for each activity
  dist <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dist = max(cumdist))
  
  # Normalise distance
  data <- data %>%
    dplyr::left_join(dist, by = "id") %>%
    dplyr::mutate(dist_scaled = cumdist / dist) %>%
    dplyr::arrange(id, cumdist)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(dist_scaled, ele, group = id, color = type), data, alpha = 0.75, lwd = 0.2) +
    scale_color_manual(name = '', values = c('darkred', 'purple', 'black')) +
    ggplot2::facet_wrap(~id, scales = ifelse(scale_free_y, "free_y", "fixed"), nrow = nr) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(1, 4), "cm")) +
    theme(legend.position = 'none') +
    theme(axis.text.y = element_text(size = 3)) +
    geom_hline(yintercept = seq(0, 3000, 1000), lty = 2, lwd = 0.1, alpha = 0.2) +
    geom_hline(yintercept = 0, col = 'grey', alpha = 0.4)
  p
}

g <- plot_elevations(data = pd, nr = 7)
# g
ggsave(filename = '~/Desktop/elevations.pdf', plot = g, width = 7.5, height = 4.5)
# Packed circles


# Dist of distances
ggplot(data = activities %>% filter(type %in% c('Ride', 'Run')),
       aes(x = distance)) +
  geom_density(aes(group = type,
                   color = type)) +
  scale_color_manual(name = '', values = c('purple', 'black')) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(rep(1, 4), "cm")) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = 'Km')

ggplot(data = activities %>% filter(type %in% c('Run'),
                                    average_speed > 5) %>%
         mutate(year = format(as.POSIXct(start_date_local), '%Y')) %>%
         filter(year != '2020'),
       aes(x = average_speed)) +
  geom_density(aes(group = year,
                   fill = year),
               alpha = 0.7) +
  scale_fill_manual(name = '',
                    values = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(3)) +
  
  ggplot2::theme_void() +
  ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank(),
                 plot.margin = ggplot2::unit(rep(1, 4), "cm")) +
  # theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = 'Km')

# Faceted cities
col <- 'darkorange'
pd <- streams %>% 
  dplyr::mutate(lon = lng) %>%
  left_join(activities %>% dplyr::select(id, start_date_local, type, city)) %>%
  filter(type == 'Run' ,
  start_date_local >= '2019-01-01',
  start_date_local <= '2019-12-31') %>%
  filter(!city %in% c('City Not Found')) %>%
  mutate(city = recode(city,
                       'Nepalgunj Sub Metropolitan City' = 'Nepalgunj',
                       'Santa Coloma de Queralt' = 'Santa Coloma\nde Queralt',
                       'Cidade de Maputo' = 'Maputo'))

ggplot(data = pd,
       aes(x = lon,
           y = lat,
           group = id)) +
  geom_path(size = 0.25, lineend = "round",
            alpha = 0.7,
            color = col) +
  theme_black() +
  theme(
    # Specify panel options
    panel.spacing = ggplot2::unit(0, "lines"), 
    strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
    plot.margin = ggplot2::unit(rep(1, 4), "cm"),
    panel.background = element_rect(fill = "black", color  =  NA),  
    panel.border = element_rect(fill = NA, color = "black"),  
    plot.background = element_rect(color = "black", fill = "black")
  ) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        panel.background=element_blank()) + 
  theme(plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill='black'), 
        legend.background= element_rect(fill="black", colour=NA),
        legend.key = element_rect(colour = NA, col = "black",
                                  size = .5, fill = 'black')) +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        # axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.spacing = unit(0, 
                             "lines"), 
        legend.justification = c(0, 0), 
        legend.position = c(0, 
                            0)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        plot.margin=unit(c(0,0,0,0), "lines")) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # theme(legend.position = 'none') +
  theme(legend.position = 'bottomright') +
  labs(x = '', y = '') +
  # coord_map() +
  facet_wrap(~city, scales = 'free', nrow = 5) +
  theme(strip.text = element_text(color = 'white', size = 0.5))
ggsave(paste0('~/Desktop/all_cities.pdf'), width = 9, height = 5)


# Email
library(mailR)
library(yaml)
pass = yaml.load_file('credentials.yaml')
pass = pass$email_password
sender <- "joebrew@gmail.com"
recipients <- c("joebrew@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Streak app was just redeployed",
          body = "www.streak.family",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "joebrew@gmail.com",            
                      passwd = pass, ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
