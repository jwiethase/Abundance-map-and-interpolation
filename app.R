rm(list = ls(all=TRUE))  
library(shiny)
library(leaflet)
library(dplyr)
library(mapview)
library(scales)

# Import the data set
data <- read.csv('OpWall_banding_data.csv') %>% group_by(common.name) %>% mutate(rarity = length(common.name)) %>% ungroup()

# Make list of unique species in the data set:
# Use common name and rarity index
Species <- data %>% dplyr::select(common.name, rarity) %>% unique()
# Sort alphabetically
Species <- Species[order(Species$common.name, decreasing = FALSE),]
# Create list for drop down menu
Species.choices <- Species %>% mutate(choices=paste(common.name, ' (', rarity, ')', sep='')) %>% 
  dplyr::select(choices) %>% unique()
# Add option to use no species at all
Species.choices <- rbind(Species.choices, 'none')


# Make the user interface
ui <- shiny::bootstrapPage(tags$head(tags$style(HTML("
                                                     .selectize-input, .selectize-dropdown {
                                                     font-size: 120%;
                                                     }
                                                     "))),  theme = "bootstrap.css",
                           tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                           
                           # Make map span the whole area
                           leaflet::leafletOutput("map", width = "100%", height = "100%"),
                           # Add a side panel for inputs
                           shiny::absolutePanel(top = 20, right = 20, width = 300,
                                                draggable = TRUE,
                                                shiny::wellPanel(
                                                  shiny::selectInput(inputId = "maptype", 
                                                                     label = h4("Map type"),
                                                                     choices = c('Esri.WorldImagery', 'Esri.WorldTopoMap', 'OpenMapSurfer.Roads', 'Esri.DeLorme', 'OpenTopoMap')),
                                                  shiny::selectInput(inputId = "species.choices", 
                                                                     label = h4("Species (rarity)"),
                                                                     choices = Species.choices), 
                                                  uiOutput("checkbox"),
                                                  
                                                  hr(),
                                                  shiny::checkboxInput("labels", "Static site labels", TRUE),
                                                  hr(),
                                                  downloadButton('downloadData', 'Download'),
                                                  helpText("Note: Hover over red circle area* to display capture numbers"),
                                                  helpText("*Circles are drawn with capture numbers rescaled from 1 to 10")),
                                                style = "opacity: 1"
                           )
                           
)

# Make the server functions
server <- function(input, output, session) {
  # Modify the checkbox options for year dependand on the subsetted dataframe
  output$checkbox <- renderUI({
    Species <- gsub("[[:space:]]\\(.*$", "", input$species.choices)
    choice <-  as.data.frame(unique(data[data$common.name %in% Species, "year"]))
    choice$year <- choice$year[order(choice$year, decreasing = TRUE)]
    checkboxGroupInput(inputId = "checkbox",
                       label = h4("Year"),
                       choices = choice$year, selected = choice$year)
  })
  
  # Filter the initial dataframe by species and year chosen
  filteredData <- shiny::reactive({
    Species <- gsub("[[:space:]]\\(.*$", "", input$species.choices)
    data <- data[data$year %in% input$checkbox, ]
    new_df <- data %>% group_by(Loc, common.name, long, lat, scientific.name, Site) %>% 
      summarize(abundance= n()) %>% ungroup() %>% dplyr::filter(grepl(Species, common.name, ignore.case = TRUE) == TRUE)
  })
  
  # Filter the initial dataframe, but retain all columns. The product will be used for the download button 
  DataDetailed <- shiny::reactive({
    Species <- gsub("[[:space:]]\\(.*$", "", input$species.choices)
    data <- data[data$year %in% input$checkbox, ]
    new_df <- data %>% dplyr::filter(grepl(Species, common.name, ignore.case = TRUE) == TRUE) %>% dplyr::select(-rarity)
  })
  
  # Make a leaflet map that won't change with the user's input
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(data) %>%  
      mapview::addLogo('https://i0.wp.com/shop.opwall.com/wp-content/uploads/2015/10/cropped-OpWall_circle_blue.png?ssl=1',
                       src= 'remote', position = 'topleft', alpha=.7,
                       width = 100, height = 100) %>%
      leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    
  })
  
  # Update above leaflet map depending on user inputs
  shiny::observe({
    sites <- data %>% dplyr::select(long, lat, Site) %>% unique()
    
    map <- leaflet::leafletProxy(map = "map", data = filteredData()) %>%
      leaflet::addProviderTiles(input$maptype,
                                options = providerTileOptions(noWrap = TRUE)) %>%
      leaflet::clearShapes() %>%
      leaflet::addCircles(lng=~long, lat=~lat,radius = ~scales::rescale(abundance, to=c(1,10))*700, weight = 1, color = "darkred",
                          fillOpacity = 0.7, label = ~paste('Number caught: ', abundance, sep='') 
      ) %>% 
      leaflet::clearMarkers() %>%
      leaflet::addMarkers(data= sites,lng=~long, lat=~lat, label = ~as.character(Site),
                          labelOptions = labelOptions(noHide = input$labels)) 
    
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
