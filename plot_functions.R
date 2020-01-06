theme_nothing <- function(){
  ggplot2::theme_void() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(1, 4), "cm")) +
#   theme(
#   # Specify panel options
#   panel.spacing = ggplot2::unit(0, "lines"), 
#   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
#   plot.margin = ggplot2::unit(rep(1, 4), "cm"),
#   panel.background = element_rect(fill = "black", color  =  NA),  
#   panel.border = element_rect(fill = NA, color = "black"),  
#   plot.background = element_rect(color = "black", fill = "black")
# ) +
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_blank()) + 
#   theme(plot.background=element_rect(fill="black"),
#         panel.background=element_rect(fill='black'), 
#         legend.background= element_rect(fill="black", colour=NA),
#         legend.key = element_rect(colour = NA, col = "black",
#                                   size = .5, fill = 'black')) +
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
        panel.grid.minor = element_blank()) 
}

make_tiny_maps <- function(activities,
                           scales = 'free',
                           labels = FALSE){
  require(googlePolylines)
  
  
  data <- activities %>%
    filter(!is.na(start_latlng1)) %>%
    decode_df
  
  # Get some summary info
  summary <- data %>%
    get_avg_location() %>%
    left_join(activities %>% dplyr::select(distance, id),
              by = 'id')

  
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
    ggplot2::geom_path(ggplot2::aes(lon, lat, group = id), data, size = 0.55, lineend = "round",
                       col = 'black') +
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
  p <- p +
    # theme_black() +
    theme_nothing() +
    labs(x = '',
         y = '')
  return(p)
    
}

make_location_maps <- function(activities,
                               starting_locations){

  
  right <- activities %>%
    mutate(starting_location_id = generate_starting_location_id(.)) %>%
    left_join(starting_locations,
              by = 'starting_location_id') %>%
    dplyr::select(city, country, id)
  left <- activities %>%
    filter(!is.na(start_latlng1)) %>%
    decode_df
  pd <- left_join(left, right, by = 'id')
  # Remove city not found
  pd <- pd %>%
    filter(!city %in% c('City Not Found'),
           !is.na(city))

  # n_cols <- length(unique(pd$city))
  # cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(n_cols)
  # cols <- sample(cols, size = n_cols)
  ggplot(data = pd,
         aes(x = lon,
             y = lat,
             group = id)) +
    geom_path(#alpha = 0.8,
              size = 0.2,
      # aes(color = city), 
      lineend = "round") +
    facet_wrap(~city,
               scales = 'free') +
    theme_nothing() +
    # scale_color_manual(name = '', values = cols) +
    theme(legend.position = 'none') +
    theme(strip.text = element_text(size = 8)) 
}


make_interactive_map <- function(activities){
  pd <- decode_df(activities = activities) %>%
    mutate(lng = lon) %>%
    filter(!is.na(lng))
  pd <- data.frame(pd)
  # coordinates(pd) <- ~lng+lat
  # proj4string(pd) <- CRS("+proj=longlat")
  # d <- pd
  # ## list of Lines per id, each with one Line in a list
  # x <- lapply(split(d, d$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
  # 
  # # the corrected part goes here:
  # lines <- SpatialLines(x)
  # data <- data.frame(id = unique(d$id))
  # rownames(data) <- data$id
  # pd <- SpatialLinesDataFrame(lines, data)
  l <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$Stamen.Watercolor)
  return(l)
}
