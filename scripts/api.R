library(plumber)

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
    
    # Unzip the file and read the CSV
    temp_dir <- tempdir()
    unzip(data_file, exdir = temp_dir)
    csv_file <- list.files(temp_dir, pattern = "*.csv", full.names = TRUE)[1]
    water_quality_data <- read_csv(csv_file)
    
    return(water_quality_data)
  } else {
    return(data.frame(error = paste("Error:", status_code(response))))
  }
}

# Function to read station data
read_station_data <- function() {
  station_df <- read_csv("station.csv") # Assuming 'station.csv' is in the working directory
  return(station_df)
}

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

# Plumber router setup
# plumber::pr() %>% pr_run(port=8000)
