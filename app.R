rm(list = ls(all=TRUE))  
library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library (lubridate)
library(raster)
library(sp)
library(rgdal)
library(data.table)
library(shinyjs)

# Make the user interface
ui <- shiny::bootstrapPage(tags$style(" #loadmessage {
                                      position: fixed;
                                      top: 0px;
                                      left: 0px;
                                      width: 100%;
                                      padding: 5px 0px 5px 0px;
                                      text-align: center;
                                      font-weight: bold;
                                      font-size: 100%;
                                      color: #000000;
                                      background-color: #ffffff;
                                      z-index: 105;}",
                                      ".test_type {font-size: 12px}",
                                      type = "text/css", "html, body {width:100%;height:100%}"),
                           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                            tags$div("Loading...",id="loadmessage")),
                           # Make map span the whole area
                           leaflet::leafletOutput("map", width = "100%", height = "100%"),
                           
                           shinyjs::useShinyjs(), # Use for toggling slide input
                           
                           # Add a side panel for inputs
                           shiny::absolutePanel(top = 20, right = 20, width = 300,
                                                draggable = TRUE,
                                                shiny::wellPanel(div(class="test_type",
                                                                     id = "tPanel",style = "overflow-y:scroll; max-height: 1000px; opacity: 1",
                                                                     shiny::fileInput(inputId = 'dataset', 
                                                                                      label = h5('Choose .csv file to upload'),
                                                                                      accept = c('.csv')),
                                                                     shiny::helpText("Warning: Dataset has to include all of the following column names:"),
                                                                     shiny::helpText("'Species' (Format: Common OR scientific)"),
                                                                     shiny::helpText("'Site'"),
                                                                     shiny::helpText("'Latitude' (Format: decimal)"),
                                                                     shiny::helpText("'Longitude' (Format: decimal)"),
                                                                     shiny::helpText("'Date' (Format: dmy)"),
                                                                     shiny::selectInput(inputId = "maptype", 
                                                                                        label = h5("Map type"),
                                                                                        choices = c('Esri.WorldImagery', 'Esri.WorldTopoMap', 'OpenMapSurfer.Roads', 'Esri.DeLorme', 'OpenTopoMap')),
                                                                     shiny::selectInput(inputId = "species.choices", 
                                                                                        label = h5("Species"),
                                                                                        choices = ' '), 
                                                                     uiOutput("checkbox"),
                                                                     hr(),
                                                                     shiny::checkboxInput("idw", "Spatial interpolation (idw)", FALSE),
                                                                     uiOutput("slider"),
                                                                     hr(),
                                                                     downloadButton('downloadData', 'Download')
                                                ))
                           )
)

# Make the server functions
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2) 
  
  data <- reactive({
    req(input$dataset)
    data <- fread(input$dataset$datapath) 
  req.names <- c("Site", "Species", "Date", "Latitude", "Longitude")
  validate(
    need(all(req.names %in% colnames(data), TRUE) == TRUE,
         message = paste("\nError: Missing or miss-spelled column names.\nUnmatched columns:\n\n", paste(c(req.names[req.names %in% colnames(data) == FALSE]), collapse="\n"), sep="")
    )
  )
  data <- data %>%
    mutate(Date = dmy(Date), 
           year = year(Date),
           Latitude = as.numeric(as.character(Latitude)),
           Longitude = as.numeric(as.character(Longitude)))
  
  # Check for non-numeric values in Latitude and Longitude column
  coordsDF <- data %>% dplyr::select(Latitude, Longitude) %>% mutate(Latitude = as.numeric(Latitude),
                                                           Longitude = as.numeric(Longitude))
  
  validate(
    need(identical(colnames(coordsDF)[colSums(is.na(coordsDF)) > 0], character(0)), TRUE,
         message = paste("\nError: Non-numeric value in column: ", paste(c(colnames(coordsDF)[colSums(is.na(coordsDF)) > 0]), collapse="\n"), sep="")
    )
  )
  remove(coordsDF)
  data
  })

observeEvent(data(), {
  Species.choices <- data() %>% dplyr::select(Species) %>% unique() %>% arrange(Species)
  updateSelectInput(session, "species.choices", choices= Species.choices$Species)
})

Spec.choice <- reactive({
  gsub("[[:space:]]\\(.*$", "", input$species.choices)
})

# Modify the checkbox options for year dependand on the subsetted dataframe

observeEvent(input$species.choices, {
  output$checkbox <- renderUI({
    data <- data()
    choice <-  data.frame(year= unique(data[data$Species %in% Spec.choice(), "year"]))
    choice$year <- choice$year[order(choice$year, decreasing = TRUE)]
    checkboxGroupInput(inputId = "checkbox",
                       label = h4("Year"),
                       choices = choice$year, selected = choice$year)
  })
})

# Filter the initial dataframe by species and year chosen
filteredData <- shiny::reactive({
  data <- data()
  data <- data[data$year %in% input$checkbox, ]
  data %>% group_by(Species, Longitude, Latitude, Site) %>% 
    summarize(abundance= n()) %>% ungroup() %>% dplyr::filter(grepl(Spec.choice(), Species, ignore.case = TRUE) == TRUE)
})

# Filter the initial dataframe, but retain all columns. The product will be used for the download button 
DataDetailed <- shiny::reactive({
  data <- data()
  data <- data[data$year %in% input$checkbox, ]
  new_df <- data %>% dplyr::filter(grepl(Spec.choice(), Species, ignore.case = TRUE) == TRUE)
})

# Make a leaflet map that won't change with the user's input
output$map <- leaflet::renderLeaflet({
  data <- data()
  leaflet::leaflet(data) %>%  
    leaflet::fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  
})
observeEvent(input$idw == TRUE,{
  toggle("slider")
})
# Update above leaflet map depending on user inputs
shiny::observe({
  data <- data()
  sites <- data %>% dplyr::select(Longitude, Latitude, Site) %>% unique()
  
  map <- leaflet::leafletProxy(map = "map", data = filteredData()) %>%
    leaflet::addProviderTiles(input$maptype,
                              options = providerTileOptions(noWrap = TRUE)) %>%
    leaflet::clearShapes() %>%
    leaflet::clearMarkers()
  if(input$idw == FALSE){
    map <- map %>% 
      leaflet::addCircles(lng=~Longitude, lat=~Latitude, radius = ~scales::rescale(abundance, to=c(1,10))*((max(Longitude+0.3) - min(Longitude-0.3))*1100), weight = 1, color = "darkred",
                          fillOpacity = 0.7, label = ~paste('Samples: ', abundance, sep='')) %>%  
      leaflet::fitBounds(~min(Longitude-.5), ~min(Latitude-.5), ~max(Longitude+.5), ~max(Latitude+.5))
    
  } else {
    
    output$slider <- renderUI({
      sliderInput("Slider", "Inverse Distance Weighting Power", min=0, max=5, value=2)
    })
    new_df <- filteredData() %>% dplyr::rename(lon = "Longitude",
                                               lat = "Latitude")
    validate(
      need(length(rownames(new_df)) > 1, "Not enough data")
    )
    observeEvent(input$Slider, {
      

      coords <- cbind(new_df$lon, new_df$lat)
      sp = sp::SpatialPoints(coords)
      spdf = sp::SpatialPointsDataFrame(sp, new_df)
      sp::proj4string(spdf) <- CRS("+init=epsg:4326")

      # Create an empty grid where n is the total number of cells
      x.range <- as.numeric(c(min(new_df$lon - 1), max(new_df$lon +
                                                         1)))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(min(new_df$lat - 1), max(new_df$lat +
                                                         1)))  # min/max latitude of the interpolation area

      extent <- data.frame(lon = c(min(new_df$lon - 0.5), max(new_df$lon +
                                                                0.5)), lat = c(min(new_df$lat - 0.5), max(new_df$lat +
                                                                                                            0.5)))
      # expand points to grid
      grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2],
                                 by = round((log(length(rownames(new_df)))) * 0.004, digits = 3)),
                         y = seq(from = y.range[1],
                                 to = y.range[2],
                                 by = round((log(length(rownames(new_df)))) * 0.004, digits = 3)))

      sp::coordinates(grd) <- ~x + y
      sp::gridded(grd) <- TRUE
      
      # Add P's projection information to the empty grid
      sp::proj4string(grd) <- sp::proj4string(spdf)

      # Run the interpolation
      P.idw <- gstat::idw(new_df$abundance ~ 1, locations = spdf, newdata = grd, idp = input$Slider)
      
    # Convert to raster object
    r <- raster::raster(P.idw)
    pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                        na.color = "transparent")
    
    map <- map %>% 
      clearImages() %>% 
      leaflet::addRasterImage(r, colors = pal, opacity = 0.8) %>%
      clearControls() %>% 
      addLegend(pal = pal, values = values(r),
                title = "Abundance", position = "bottomleft") %>%  
      leaflet::fitBounds(~min(Longitude-.2), ~min(Latitude-.2), ~max(Longitude+.2), ~max(Latitude+.2))
    })
  }
    map <- map %>% 
      leaflet::addMarkers(data= sites,lng=~Longitude, lat=~Latitude, label = ~as.character(Site),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(noHide = TRUE),
                          popup = paste("Latitude:", ~Latitude, "<br>",
                                        "Longitude:", ~Longitude))

})

# Download the filtered dataframe
output$downloadData <- downloadHandler(
  filename = function() { 
    paste(input$species.choices, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(DataDetailed(), file, row.names = FALSE)
  })
}

shiny::shinyApp(ui, server)
