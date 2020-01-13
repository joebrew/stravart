library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)

leaflet() %>%
  setView(0, 0, 2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addDrawToolbar(
    targetGroup = "draw",
    singleFeature = TRUE,
    polylineOptions = FALSE,
    polygonOptions = FALSE,
    circleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    rectangleOptions = drawRectangleOptions(),
  #   editOptions = editToolbarOptions(
  #     selectedPathOptions = selectedPathOptions()
  #   )
  # )  %>%
  editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) %>%
  # editOptions = FALSE) %>%
  addLayersControl(
    overlayGroups = c("draw"),
    options = layersControlOptions(collapsed = FALSE)
  ) 
