library(shiny)
library(tidyverse)
library(sf)
library(httr)
library(xml2)
library(raster)
library(ggspatial)

# Function to fetch and process water quality data
fetch_water_quality_data <- function(state_code, characteristic_name) {
  url <- paste0("https://www.waterqualitydata.us/Result/search?statecode=", state_code, "&characteristicName=", characteristic_name, "&mimeType=csv&zip=yes")
  response <- GET(url)

  if (status_code(response) == 200) {
    data_file <- "water_quality_data.zip"
    writeBin(content(response, "raw"), data_file)
    temp_dir <- tempdir()
    unzip(data_file, exdir = temp_dir)
    csv_file <- list.files(temp_dir, pattern = "*.csv", full.names = TRUE)[1]
    water_quality_data <- read_csv(csv_file)

    return(water_quality_data)
  } else {
    return(tibble(error = paste("Error:", status_code(response))))
  }
}

# UI
ui <- fluidPage(
  titlePanel("Water Quality Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stateCode", "Select State Code:", choices = c("US:08" = "US:08")),
      selectInput("charName", "Select Characteristic Name:", choices = c("Ammonia" = "Ammonia")),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("waterQualityPlot")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$submit, {
    processed_data <- fetch_water_quality_data(input$stateCode, input$charName)
    
    output$waterQualityPlot <- renderPlot({
      if ("error" %in% names(processed_data)) {
        plot(1, type = "n", xlab = "", ylab = "", main = processed_data$error[1])
      } else {
        ggplot(processed_data, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
          geom_point() +
          labs(title = "Ammonia Concentration Over Time", x = "Date", y = "Ammonia Concentration (mg/L)") +
          theme_minimal()
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
