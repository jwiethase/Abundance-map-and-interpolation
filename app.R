rm(list = ls(all=TRUE))  
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(scales)
library (lubridate)
library(raster)
library(sp)
library(rgdal)
library(data.table)
library(shinyjs)
library(shinyBS)
library(DT)

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
                           shiny::absolutePanel(top = 20, right = 20, width = 330, 
                                                div(style = "display:inline-block;width:100%;text-align: right;",
                                                    bsButton("showpanel", " ", type = "toggle", value = TRUE, icon = icon("angle-double-down", lib = "font-awesome"))),
                                                draggable = FALSE,
                                                shiny::wellPanel(id = "Sidebar",
                                                                 div(class="test_type",
                                                                     id = "tPanel",style = "overflow-y:scroll;overflow-x: hidden;
                                                                     max-height: 800px; max-width: 330px;opacity: 1",
                                                                     uiOutput("out"),
                                                                     shiny::fileInput(inputId = 'dataset', 
                                                                                      label = h5('Choose .csv file to upload'),
                                                                                      accept = c('.csv')),
                                                                     shiny::helpText("Warning: Dataset has to include all of the following column names:"),
                                                                     shiny::helpText("'Species'"),
                                                                     shiny::helpText("'Latitude' (Format: decimal)"),
                                                                     shiny::helpText("'Longitude' (Format: decimal)"),
                                                                     shiny::helpText("OPTIONAL 'Site'"),
                                                                     shiny::helpText("OPTIONAL 'Date' (Format: dmy)"),
                                                                     shiny::selectInput(inputId = "species.choices", 
                                                                                        label = h5("Species"),
                                                                                        choices = ' '), 
                                                                     uiOutput("checkbox"),
                                                                     hr(),
                                                                     splitLayout(
                                                                       shiny::checkboxInput("idw", "Interpolation (idw)", FALSE),
                                                                       shiny::checkboxInput("circles", "Circle markers", FALSE)
                                                                     ),
                                                                     splitLayout(
                                                                       shiny::checkboxInput("cluster", "Clustered markers", FALSE),
                                                                       shiny::checkboxInput("labels", "Static labels", TRUE)
                                                                       ),
                                                                     uiOutput("sliderCircle"),
                                                                     uiOutput("slider"),
                                                                     hr(),
                                                                     downloadButton('downloadData', 'Download')
                                                ))
                           ),
                           shinyjs::hidden(
                             div(
                               id = "cp1",
                               conditionalPanel("input.map_shape_click",
                                                absolutePanel(top = 50, bottom = 50, right = 50, left = 70, height = 500, width = 1200, 
                                                              div(style = "display:inline-block;width:100%;text-align: right;",
                                                                  actionButton("close", "x")),
                                                              wellPanel(div(id = "tablepanel",
                                                                            style =  "overflow-y: scroll;overflow-x: scroll; max-height: 500px; max-width: 1200px",
                                                                            DTOutput("clickInfo")
                                                                            )
                                                                        ), draggable = TRUE
                                                              )
                                                )
                               )
                           )
            
)

# Make the server functions
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2) 
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    }
  })
  
  observeEvent(input$map_shape_click,{
    shinyjs::show("cp1")
  })
  observeEvent(input$close,{
    shinyjs::hide("cp1")
  })
  

  data <- reactive({
    req(input$dataset)
    data <- fread(input$dataset$datapath) 
    req.names <- c("Species", "Latitude", "Longitude")
    validate(
      need(all(req.names %in% colnames(data), TRUE) == TRUE,
           message = paste("\nError: Missing or miss-spelled column names.\nUnmatched columns:\n\n", paste(c(req.names[req.names %in% colnames(data) == FALSE]), collapse="\n"), sep="")
      )
    )
    data <- data %>%
      mutate(Latitude = as.numeric(as.character(Latitude)),
             Longitude = as.numeric(as.character(Longitude)))
    
    if("Site" %in% names(data) == FALSE){
    data$Site <- data %>%
        group_by(Latitude, Longitude) %>% 
        group_indices()
    }
    
    if("Date" %in% names(data)){
      data <- data %>%
        mutate(Date = dmy(Date),
               year = year(Date))
    }
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
    if("Date" %in% names(data())){
    output$checkbox <- renderUI({
      data <- data()
      choice <-  data.frame(year= unique(data[data$Species %in% Spec.choice(), "year"]))
      choice$year <- choice$year[order(choice$year, decreasing = TRUE)]
      checkboxGroupInput(inputId = "checkbox",
                         label = h4("Year"),
                         choices = choice$year, selected = choice$year)
    })
    }
  })
  
  # Filter the initial dataframe by species and year chosen
  filteredData <- shiny::reactive({
    data <- data()
    if("Date" %in% names(data)){
    data <- data[data$year %in% input$checkbox, ]
    }
    data %>% group_by(Species, Longitude, Latitude, Site) %>% 
      summarize(abundance= n()) %>% ungroup() %>% dplyr::filter(grepl(Spec.choice(), Species, ignore.case = TRUE) == TRUE)
  })
  
  # Filter the initial dataframe, but retain all columns. The product will be used for the download button 
  DataDetailed <- shiny::reactive({
    data <- data()
    if("Date" %in% names(data)){
    data <- data[data$year %in% input$checkbox, ]
    }
    new_df <- data %>% dplyr::filter(grepl(Spec.choice(), Species, ignore.case = TRUE) == TRUE)
  })
  
  # Make a leaflet map that won't change with the user's input
  output$map <- leaflet::renderLeaflet({
    data <- data()
    leaflet::leaflet(data) %>%  
      leaflet::fitBounds(~min(Longitude+.1), ~min(Latitude), ~max(Longitude+.1), ~max(Latitude)) %>%  
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addProviderTiles(providers$OpenMapSurfer.Roads, group = "OpenMapSurfer.Roads") %>%
      addProviderTiles(providers$Esri.DeLorme, group = "Esri.DeLorme") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>% 
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Zoom to Level 1",
        onClick = JS("function(btn, map){ map.setZoom(1);}"))) %>% 
      leaflet.extras::addSearchOSM() %>% 
      leaflet.extras::addFullscreenControl() %>%
      addLayersControl(
        baseGroups = c('Esri.WorldImagery', 'Esri.WorldTopoMap', 'OpenMapSurfer.Roads', 'Esri.DeLorme', 'OpenTopoMap', "OpenStreetMap.Mapnik"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft"
      ) 

    
  })
  observeEvent(input$idw == TRUE,{
    toggle("slider")
  })
  observeEvent(input$circles == TRUE,{
    toggle("sliderCircle")
  })
  # Update above leaflet map depending on user inputs
  
  shiny::observe({
    data <- data()
    sites <- data %>% dplyr::select(Longitude, Latitude, Site) %>% unique()
    
    map <- leaflet::leafletProxy(map = "map", data = filteredData())  
    if(input$circles == TRUE){
      map <- map  %>%  
        leaflet::fitBounds(~min(Longitude+.5), ~min(Latitude-.5), ~max(Longitude+.5), ~max(Latitude+.5))
      
      output$sliderCircle <- renderUI({
        sliderInput("circleSlider", "Circle size", min=10, max=2000, step = 10, value=1100)
      })
      observeEvent(input$circleSlider, {
      map <- map %>% 
        clearImages() %>% 
        clearShapes() %>% 
        leaflet::addCircles(lng=~Longitude, lat=~Latitude, radius = ~scales::rescale(abundance, to=c(1,10))*((max(Longitude+0.3) - min(Longitude-0.3))*input$circleSlider), weight = 1, color = "darkred",
                            fillOpacity = 0.7, label = ~paste('Samples: ', abundance, sep=''),
                            highlight = highlightOptions(
                              weight = 3,
                              color = "black",
                              opacity = 1.0,
                              bringToFront = TRUE,
                              sendToBack = TRUE),
                            layerId = ~Site)
      })
    } else {
      map <- map %>% 
        clearShapes()
    }
    
    if(input$idw == TRUE){ 
      map <- map  %>%  
        leaflet::fitBounds(~min(Longitude+.2), ~min(Latitude-.2), ~max(Longitude+.2), ~max(Latitude+.2))
      
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
        
        # Run the interpolation f
        P.idw <- gstat::idw(new_df$abundance ~ 1, locations = spdf, newdata = grd, idp = input$Slider)
        
        # Convert to raster object
        r <- raster::raster(P.idw)
        pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                            na.color = "transparent")
        
        map <- map %>% 
          clearImages() %>% 
          clearShapes() %>% 
          leaflet::addRasterImage(r, colors = pal, opacity = 0.8) %>%
          clearControls() 
      })
    } else {
      map <- map %>% 
        clearImages()
    }
    
    observeEvent({
      input$labels
      input$cluster
    }, {
      map <- map %>% 
        clearMarkers() %>% 
        clearControls() %>% 
        clearMarkerClusters() 
      if(input$cluster == TRUE){
        map <- map %>% 
          leaflet::addMarkers(data= sites,lng=~Longitude, lat=~Latitude, label = ~as.character(Site),
                              clusterOptions = markerClusterOptions(),
                              labelOptions = labelOptions(noHide = input$labels),
                              layerId = ~Site) 
      } else {
        map <- map %>% 
          leaflet::addMarkers(data= sites,lng=~Longitude, lat=~Latitude, label = ~as.character(Site),
                              labelOptions = labelOptions(noHide = input$labels),
                              layerId = ~Site)
      }
    })
  })
  
  
  output$out <- renderPrint({
    validate(need(input$map_click, FALSE))
    output$out <- renderUI({
      df <- input$map_click
      textInput("Coords", "Clicked coordinates:", value = paste(round(df[[1]], digits= 4), ", ", round(df[[2]], digits= 4), sep = ""))
    })
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    leafletProxy('map') %>%
      removeMarker(layerId = click$id) %>% 
      addMarkers(data = click, lng=~lng, lat=~lat, layerId = ~id,
                 icon = makeAwesomeIcon(icon = "home", library = "glyphicon",
                                        markerColor = "red", iconColor = "white", spin = FALSE,
                                        extraClasses = NULL, squareMarker = FALSE, iconRotate = 0,
                                        fontFamily = "monospace", text = NULL) )
  })
    
  observeEvent(input$map_shape_click, {
    data <- data()
    click <- input$map_shape_click
    data <- data %>% filter(Site == click$id,
                            grepl(Spec.choice(), Species, ignore.case = TRUE) == TRUE)
    output$clickInfo <- renderDT({data}, options = list(scrollX = FALSE, paging = FALSE))
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
