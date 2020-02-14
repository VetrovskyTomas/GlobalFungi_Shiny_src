# Function for module UI with SH
resutsMapUI <- function(id) {
  ns <- NS(id)
  #world map...
  sidebarPanel(width = "100%", style = "background-color:white;", 
      fluidRow(  
        #this will create a space for us to display our map
        column(10,leafletOutput(ns("mymap"))),
        #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
        column(2, checkboxInput(ns("all_samples"), "Show all samples", FALSE))
      ),
      fluidRow(  
      uiOutput(ns("map_sample_info"))
    )
  )
}

# Function for module server logic
resutsMapFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns

  samples <- isolate(variable$samples)
  
  # actual version - map of samples from the study...
  observe({
    print("MAP REFRESH...")
    map_data <- NULL
    # TAXON
    if (!is.null(samples$abundances)){
      map_data <- data.frame(id = samples[,"id"],
                           longitude = samples[,"longitude"],
                           latitude = samples[,"latitude"],
                           type = samples$abundances / samples$ITS_total,#cut((samples$abundances / samples$ITS_total)*100.0, breaks=c(0.0, 0.1, 1.0, 10.0, 100.0)),
                           stringsAsFactors = F)
    
    map_data <- map_data[order(map_data$type),]
    #print(map_data)
    pallete <- colorBin(palette = c('#f0f0f0', '#ffce00', '#ff5a00', '#8b0000'), map_data$type, bins = c(0, .001, .01, .1, 1))
    

    # map of samples from the study...
    output$mymap <- renderLeaflet({
      if (input$all_samples) {
      leaflet(data = map_data, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
        addProviderTiles(providers$Esri, options = providerTileOptions(
          updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
          updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
        ) %>%
        addCircleMarkers(data = global_samples, ~longitude, ~latitude,  options = pathOptions(clickable = FALSE),layerId = -1,
                         color = "gray",
                         radius = 4,
                         fillColor = "gray",
                         weight = 1,
                         stroke = T,
                         fillOpacity = 1
        ) %>%
        addCircleMarkers(data = map_data, ~longitude, ~latitude, layerId = ~id,
                         color = "black",
                         radius = 4,
                         fillColor = ~pallete(map_data$type),
                         weight = 1,
                         stroke = T,
                         fillOpacity = 1
        ) %>%
        addLegend("topright", pal = pallete, values = map_data$type,
                  labFormat = labelFormat(
                    suffix = " %", between = " - ",
                    transform = function(x) 100 * x
                  ),
                  title = "Abundance", opacity = 1)
      } else {
        leaflet(data = map_data, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
          addProviderTiles(providers$Esri, options = providerTileOptions(
            updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
            updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
          ) %>%
          addCircleMarkers(data = map_data, ~longitude, ~latitude, layerId = ~id,
                           color = "black",
                           radius = 4,
                           fillColor = ~pallete(map_data$type),
                           weight = 1,
                           stroke = T,
                           fillOpacity = 1
          ) %>%
          addLegend("topright", pal = pallete, values = map_data$type,
                    labFormat = labelFormat(
                      suffix = " %", between = " - ",
                      transform = function(x) 100 * x
                    ),
                    title = "Abundance", opacity = 1)
      }
    })
    } else {
      # STUDY
      map_data <- data.frame(id = samples[,"id"],
                             longitude = samples[,"longitude"],
                             latitude = samples[,"latitude"],
                             #type = 0.0,#cut((samples$abundances / samples$ITS_total)*100.0, breaks=c(0.0, 0.1, 1.0, 10.0, 100.0)),
                             stringsAsFactors = F)
      
      # map of samples from the study...
      output$mymap <- renderLeaflet({
        if (input$all_samples) {
          leaflet(data = map_data, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
            addProviderTiles(providers$Esri, options = providerTileOptions(
              updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
              updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
            ) %>%
            addCircleMarkers(data = global_samples, ~longitude, ~latitude,  options = pathOptions(clickable = FALSE),layerId = -1,
                             color = "gray",
                             radius = 4,
                             fillColor = "gray",
                             weight = 1,
                             stroke = T,
                             fillOpacity = 1
            ) %>%
            addCircleMarkers(data = map_data, ~longitude, ~latitude, layerId = ~id,
                             color = "black",
                             radius = 4,
                             fillColor = "blue",
                             weight = 1,
                             stroke = T,
                             fillOpacity = 1
            )
        } else {
          leaflet(data = map_data, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
            addProviderTiles(providers$Esri, options = providerTileOptions(
              updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
              updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
            ) %>%
            addCircleMarkers(data = map_data, ~longitude, ~latitude, layerId = ~id,
                             color = "black",
                             radius = 4,
                             fillColor = "blue",
                             weight = 1,
                             stroke = T,
                             fillOpacity = 1
            )
        }
      })
    }

    click <- NULL
    
    observeEvent(input$mymap_marker_click,{
      click <- input$mymap_marker_click
      if (!is.null(click)) {
        sample_info <- samples[which(samples$id %in% click$id),]
        
        text <- paste0("id-",sample_info[,"id"], " (", sample_info[,"ITS_total"]," ITS)")
        if("abundances" %in% colnames(samples)) {
          text <- paste0("id-",sample_info[,"id"], " (", sample_info[,"abundances"],"/",sample_info[,"ITS_total"],")")
        } 
        
        leafletProxy(mapId = "mymap") %>%
          clearPopups() %>%
          addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
        
        # map sample details - clicked marker...
        output$map_sample_info <- renderUI({
          req(length(click) > 0)
          resultsSampleUI(id = ns("results_sample"))
        })
        
        callModule(module = resultsSampleFunc, id = "results_sample", isolate(click$id))
      }
      }, ignoreInit = TRUE
    )
    
    observe({
      # map sample details - clicked marker...
      if (is.null(click)) {
      output$map_sample_info <- renderUI({
        req(length(click) > 0)
        resultsSampleUI(id = ns("results_sample"))
      })
      callModule(module = resultsSampleFunc, id = "results_sample", -1)
      }
    })
    
  })
  
}