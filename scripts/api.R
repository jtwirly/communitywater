library(plumber)
# load packages
library(tidyverse)

# spatial packages
library(sf)
library(httr)
library(xml2)

library(leaflet)
library(dplyr)
library(htmlwidgets) 

# Function to fetch and process water quality data
fetch_water_quality_data <- function(state_code, characteristic_name) {
  url <- paste0("https://www.waterqualitydata.us/Result/search?statecode=", 
                state_code, 
                "&characteristicName=", 
                characteristic_name, 
                "&mimeType=csv&zip=yes")

  response <- GET(url)

  if (status_code(response) == 200) {
  data_file <- "water_quality_data.zip"
  writeBin(content(response, "raw"), data_file)
  cat("Data downloaded successfully.\n")
  
  # Unzip the file and read the CSV
  temp_dir <- tempdir()
  unzip(data_file, exdir = temp_dir)
  csv_file <- list.files(temp_dir, pattern = "*.csv", full.names = TRUE)[1]
  water_quality_data <- read_csv(csv_file)
  
  # Plot the data
  ggplot(water_quality_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point() +
    labs(title = "Ammonia Concentration Over Time",
         x = "Date",
         y = "Ammonia Concentration (mg/L)") +
    theme_minimal()
} else {
  cat(paste("Error:", status_code(response)), "\n")
}

# bring in the station data 
# may need to modify the path based on your working directory
station_df <- read_csv("station.csv")

station_sub_df <- station_df %>%
  dplyr::select(
    MonitoringLocationIdentifier,
    MonitoringLocationName,
    MonitoringLocationTypeName,
    LatitudeMeasure,
    LongitudeMeasure,
    HorizontalCoordinateReferenceSystemDatumName,
    `VerticalMeasure/MeasureValue`,
    `VerticalAccuracyMeasure/MeasureUnitCode`,
    CountyCode
  )

# merge data frames 

water_quality_sub_df <- water_quality_data %>% 
  dplyr::select(
    OrganizationFormalName,
    ActivityMediaSubdivisionName,
    ActivityStartDate,
    `ActivityStartTime/Time`,
    ActivityEndDate,
    `ActivityEndTime/Time`,
    MonitoringLocationIdentifier,
    HydrologicCondition,
    HydrologicEvent,
    CharacteristicName,
    ResultDetectionConditionText,
    `SampleCollectionMethod/MethodName`,
    ResultMeasureValue,
    `ResultMeasure/MeasureUnitCode`
  )


water_quality_wloc_df <- left_join(water_quality_sub_df, station_sub_df)
write_csv(water_quality_wloc_df, "water_quality_wloc_colorado.csv")

# Visualizing Stations -----------

library(raster)
us <- getData("GADM",country="USA",level=1)

us.states <- us[us$NAME_1 %in% "Colorado",]

library(sf)

water_quality_sf <- water_quality_wloc_df %>% 
  filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure)) %>%
  st_as_sf(coords = c( "LongitudeMeasure", "LatitudeMeasure"), crs = "epsg:4269") %>% 
  mutate(value = as.numeric(ResultMeasureValue))

CO_sf <- us.states %>% 
  st_as_sf(crs = "epsg:4326") %>% 
  st_transform(st_crs(water_quality_sf))

CO_sf_3857 <- st_transform(CO_sf, crs = "epsg:3857")
water_quality_sf_3857 <- st_transform(water_quality_sf, crs = "epsg:3857")

ggplot() + 
  ggspatial::annotation_map_tile(type = "hotstyle", zoom = 7) +
  geom_sf(data = water_quality_sf_3857, aes(shape = MonitoringLocationTypeName, 
                                            fill = MonitoringLocationTypeName)) +
  geom_sf(data = CO_sf_3857, fill = NA) +
  guides(shape = guide_legend(ncol = 2, title = "Location Type"),
         fill = guide_legend(ncol = 2, title = "Location Type")) +
  scale_shape_manual(name = "Location Type", 
                     values = c(2, 3, 1, 15, 21, 25, 13, 8)) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave("colorado_water_quality_overview.png", units = "in", width = 6, height = 6)
# filter based on thresholds
# for Ammonia, acute threshold is 17 mg/L ; 1.9 mg/L chronic (30-day average)


# now make a plot with the max N value for each location

water_quality_3857_sum_sf <- water_quality_sf_3857 %>% 
  group_by(MonitoringLocationIdentifier, MonitoringLocationTypeName) %>%
  summarize(max_N = max(value))

ggplot() + 
  ggspatial::annotation_map_tile(type = "hotstyle", zoom = 7) +
  geom_sf(data = water_quality_3857_sum_sf, aes(fill = max_N, size = max_N), shape = 21) +
  geom_sf(data = CO_sf_3857, fill = NA) +
  scale_size_continuous(name = "Max N") +
  scale_fill_stepsn(name = "Max N", breaks = c(0, 5, 10, 15, 20), colors = RColorBrewer::brewer.pal(5, "Purples")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggspatial::annotation_north_arrow(pad_y = unit(0.75, "cm")) +
  ggspatial::annotation_scale()

ggsave("colorado_ammonia_max.png", units = "in", width = 6, height = 6)

# Function to create a leaflet map
create_map <- function() {
  # Assuming water_quality_wloc_df is globally accessible
  map <- leaflet(water_quality_wloc_df) %>%
    addTiles() %>%
    setView(lng = -105.7821, lat = 39.5501, zoom = 7)

  if(nrow(water_quality_wloc_df) > 0) {
    for (i in 1:nrow(water_quality_wloc_df)) {
      map <- map %>% addMarkers(
        lng = water_quality_wloc_df$LongitudeMeasure[i], 
        lat = water_quality_wloc_df$LatitudeMeasure[i],
        popup = paste("Ammonia Level:", water_quality_wloc_df$ResultMeasureValue[i])
      )
    }
  }

  return(map)
}

# Function to set CORS headers
handleCORS <- function(req, res) {
  res$setHeader('Access-Control-Allow-Origin', '*')
  plumber::forward()
}

# Register the middleware
pr <- plumber::pr()
pr$registerHooks(list(
  'plumber.middleware' = handleCORS
))

#* @apiTitle Water Quality Monitoring API

#* Endpoint for water quality data
#* @get /water_quality
function(state="US:08", characteristic="Ammonia") {
  fetch_water_quality_data(state, characteristic)
}

#* Endpoint for station data
#* @get /stations
function() {
  read_station_data()
}

# Endpoint to serve the leaflet map
#* @get /map
function(res) {
  map <- create_map()
  htmlwidgets::saveWidget(map, "map.html", selfcontained = TRUE)
  plumber::forward("map.html")
}

# Plumber router setup
pr <- plumber::pr()
pr$run(port=8000)