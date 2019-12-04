library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load fulcrum_dat.Rda to see processed collection data
load("/Users/tim/repos/20191001_Hawaii/data/fulcrum/fulcrum_dat.Rda")

# filter to collection_lat_long_method_diff = NA
fulcrum_lat_long <- fulcrum_dat %>%
  dplyr::filter(collection_lat_long_method == "fulcrum", !is.na(collection_id)) %>%
  dplyr::distinct(collection_id, .keep_all = T)

# load Exif data to see if lat longs are really missing
load("/Users/tim/repos/20191001_Hawaii/data/fulcrum/exif.Rda")

exif_lat_long_NA <- exif %>%
  dplyr::filter(is.na(latitude))

# yes they are missing
#we use fulcrum lat and long then we need a manual check on location data from person that did the collections
# filter collection data to fulcrum photo method.
# plot these location data for review. need to make useful plot with zoom most likely
photo_lat_long_NAs <- dplyr::left_join(exif_lat_long_NA, fulcrum_lat_long, by = "sample_photo")

###################################################################################
### Diagnostic  Map function                                                    ###
###################################################################################
map_collection <- function(df, color_use) {
  
  icos <- iconList(
    red = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/red.svg"),
      iconWidth = 20, iconHeight = 20,
      popupAnchorX = 0.001, popupAnchorY = -20,
      iconAnchorX = 20/2, iconAnchorY = 20
    ),
    green = makeIcon(
      iconUrl = paste0("https://storage.googleapis.com/andersenlab.org/img/green.svg"),
      iconWidth = 10, iconHeight = 10,
      popupAnchorX = 0.001, popupAnchorY = -10,
      iconAnchorX = 10/2, iconAnchorY = 10
  )
)
  df <- dplyr::filter(df, !is.na(df[[color_use]])) %>%
    dplyr::mutate(substrate=ifelse(is.na(substrate), "", substrate)) %>%
    dplyr::arrange(collection_lat_long_method)
  
  #print(df)
  
  # make title
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))
  
  title <- tags$div(
    tag.map.title, HTML(glue::glue("Review {df %>% dplyr::distinct(project_id) %>% dplyr::pull(project_id)} collection locations"))
  )  
  
  attach(df)
  leaflet::leaflet(data = df, width = 1200, height = 800, options = list(zoomControl = T)) %>% 
    leaflet::addTiles( 
      paste0( 
        "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}.png",
        jsonlite::read_json("data/thunderforest.json")$key)  
    ) %>%
    leaflet::addMarkers(~collection_longitude,
                        ~collection_latitude,
                        popup = glue::glue("<h2>{collection_id}</h2><hr />
                                           <strong>collection uplaoded by:</strong> {collection_by}<br />
                                           <strong>latitidue, longitude:</strong> {format(round(collection_latitude, 6), nsmall = 6)}, {format(round(collection_longitude, 6), nsmall = 6)}<br />
                                           <strong>postion method used:</strong> {collection_lat_long_method}<br />
                                           <strong>local time:</strong> {collection_local_time}<br />
                                           <strong>altitude:</strong> {altitude} meters<br />
                                           <strong>landscape:</strong> {landscape}<br /><br />"),
                        popupOptions(maxWidth = 500),
                        icon = icos[ df[[color_use]] ] ) %>%
    leaflet::addControl(title, position = "topleft", className="map-title")
  
  #htmlwidgets::saveWidget(m, tempfile(), selfcontained = FALSE)
  #webshot::webshot("temp.html", file = "map.png",
  #        cliprect = "viewport", vwidth = 1000, vheight = 1000)
}

##########################################################################
### Use the map function                                               ###
##########################################################################
# Make df for diagnostic plotting
test_df <- fulcrum_dat %>%
  dplyr::filter(!is.na(collection_id)) %>%
  dplyr::distinct(collection_id, .keep_all =T)

# map collection for diagnostic
diagnostic_map <-  map_collection(test_df %>% dplyr::mutate(color = case_when(collection_lat_long_method == "fulcrum" ~ "red",
                                                                              collection_lat_long_method == "photo" ~ "green")), "color")
# save it
saveWidget(diagnostic_map, file="collection_location_diagnostic_map.html")

# postion diff plot
log_mean_diff <- mean(log10(test_df$collection_lat_long_method_diff), na.rm = T)   

method_histogram <- ggplot(test_df) +
  aes(x = log10(collection_lat_long_method_diff)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = mean(log10(test_df$collection_lat_long_method_diff), na.rm = T), lty = 2, color = "red") +
  annotate("text", x = 2.5, y = 15, label = glue::glue("mean difference = {format(round(10^(log_mean_diff), 2), nsmall = 2)} meters")) +
  theme_bw() +
  labs(x = "Distance between fulcrum and photo position methods (log10)")

ggsave(method_histogram, filename = 'plots/collection_location_method_histogram.png', width = 7.5, height = 5)


