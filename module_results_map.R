# Function for module UI with SH
resutsMapUI <- function(id) {
  ns <- NS(id)
  #world map...
  sidebarPanel(width = "100%", style = "background-color:white;", 
      fluidRow(  
        #this will create a space for us to display our map
        column(12,leafletOutput(ns("mymap"), height=600))
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

  filtered <- isolate(variable$filter)
  samples <- isolate(variable$samples)
  
  # actual version - map of samples from the study...
  observe({
    # clean map
    if (!filtered) {
      leafletProxy(mapId = "mymap") %>% clearPopups() %>% clearMarkers() %>% clearControls() %>% setView(lat = 0, lng = 0, zoom = 0)
    }
    print(paste0("MAP REFRESH...", nrow(samples), " FILTERED ",filtered))
    map_data <- NULL
    pallete <- NULL
    # TAXON
    if (!is.null(samples$abundances)){
      map_data <- data.frame(id = samples[,"id"],
                           longitude = samples[,"longitude"],
                           latitude = samples[,"latitude"],
                           type = samples$abundances / samples$ITS_total,
                           stringsAsFactors = F)
    
      map_data <- map_data[order(map_data$type),]
      pallete <- colorBin(palette = c('#f0f0f0', '#ffce00', '#ff5a00', '#8b0000'), map_data$type, bins = c(0, .001, .01, .1, 1))
    } else {
      map_data <- data.frame(id = samples[,"id"],
        longitude = samples[,"longitude"],
        latitude = samples[,"latitude"],
        type = 1,
        stringsAsFactors = F)
        pallete <- colorBin(palette = c('blue'), map_data$type, bins = c(0,1))
    }

    # map of samples from the study...
    output$mymap <- renderLeaflet({
        leaflet(data = map_data, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
          addProviderTiles(providers$Esri, options = providerTileOptions(
            updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
            updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
            ,group = "default") %>%
        addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(
          updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
          updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
          , group = "foto") %>%
          addMapPane("background", zIndex = 410) %>%
          addMapPane("foreground", zIndex = 420) %>%
          addCircleMarkers(data = global_samples, ~longitude, ~latitude,  options = pathOptions(clickable = FALSE, pane = "background"),layerId = -1,
                           color = "gray",
                           radius = 4,
                           fillColor = "darkgray",
                           weight = 2,
                           stroke = T,
                           fillOpacity = 1,
                           group = "all samples"
          ) %>% hideGroup("all samples") %>%
          addCircleMarkers(data = map_data, ~longitude, ~latitude, options = pathOptions(pane = "foreground") ,layerId = ~id,
                           color = "black",
                           radius = 4,
                           fillColor = ~pallete(map_data$type),
                           weight = 2,
                           stroke = T,
                           fillOpacity = 1,
                           group = "selected samples"
          ) %>% fitBounds(~min(map_data$longitude)-0.001, ~min(map_data$latitude)-0.001, ~max(map_data$longitude)+0.001, ~max(map_data$latitude)+0.001) %>% 
          addLegend("topright", pal = pallete, values = map_data$type,
                    labFormat = labelFormat(
                      suffix = " %", between = " - ",
                      transform = function(x) 100 * x
                    ),
                    title = "Abundance", opacity = 1)  %>%
          # Layers control
          addLayersControl(
            baseGroups = c("default", "foto"),
            overlayGroups = c("all samples"),
            options = layersControlOptions(collapsed = FALSE)
          )
    })
    
    click <- NULL
    
    observeEvent(input$mymap_marker_click,{
      click <- input$mymap_marker_click
      if (!is.null(click)) {
        sample_info <- samples[which(samples$id %in% click$id),]
        
        text <- paste0(sample_info[,"permanent_id"], " (", sample_info[,"ITS_total"]," ITS)")
        if("abundances" %in% colnames(samples)) {
          text <- paste0(sample_info[,"permanent_id"], " (", sample_info[,"abundances"],"/",sample_info[,"ITS_total"],")")
        } 
        
        leafletProxy(mapId = "mymap") %>%
          clearPopups() %>%
          addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
        
        # map sample details - clicked marker...
        output$map_sample_info <- renderUI({
          req(length(click) > 0)
          callModule(module = resultsSampleFunc, id = "results_sample", isolate(click$id))
          resultsSampleUI(id = ns("results_sample"))
        })
      }
      }, ignoreInit = TRUE
    )
    
    observe({
      # map sample details - clicked marker...
      if (is.null(click)) {
      output$map_sample_info <- renderUI({
        req(length(click) > 0)
        callModule(module = resultsSampleFunc, id = "results_sample", -1)
        resultsSampleUI(id = ns("results_sample"))
      })
      }
    })
    
  })
  
}