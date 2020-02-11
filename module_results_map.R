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
  

  # actual version - map of samples from the study...
  observe({
    map_data <- NULL
    # TAXON
    if (!is.null(variable$samples$abundances)){
      map_data <- data.frame(id = variable$samples[,"id"],
                           longitude = variable$samples[,"longitude"],
                           latitude = variable$samples[,"latitude"],
                           type = variable$samples$abundances / variable$samples$ITS_total,#cut((variable$samples$abundances / variable$samples$ITS_total)*100.0, breaks=c(0.0, 0.1, 1.0, 10.0, 100.0)),
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
      map_data <- data.frame(id = variable$samples[,"id"],
                             longitude = variable$samples[,"longitude"],
                             latitude = variable$samples[,"latitude"],
                             #type = 0.0,#cut((variable$samples$abundances / variable$samples$ITS_total)*100.0, breaks=c(0.0, 0.1, 1.0, 10.0, 100.0)),
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
  })
  
  # pop up text when clicked on map...
  observe({
    click <- input$mymap_marker_click
    if (is.null(click)) {
      print("null click")
      return()
    }
    #print(click)
    sample_info <- variable$samples[which(variable$samples$id %in% input$mymap_marker_click$id),]
    
    text <- paste0(sample_info[,"id"], " (", sample_info[,"ITS_total"]," ITS)")
    if("abundances" %in% colnames(variable$samples)) {
      text <- paste0(sample_info[,"id"], " (", sample_info[,"abundances"],"/",sample_info[,"ITS_total"],")")
    } 
    
    leafletProxy(mapId = "mymap") %>%
      clearPopups() %>%
      addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
  })
  
  #
  output$map_sample_table <- renderText({
    sample_vals <- global_samples[which(global_samples$id %in% input$mymap_marker_click$id),]
    #paste0(toString(sample_vals$id)," ",toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
    paste(input$mymap_marker_click$id,toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
  })
  
  # map sample details - clicked row...
  output$map_sample_info <- renderUI({
    req(length(input$mymap_marker_click) > 0)
    resultsSampleUI(id = ns("results_sample"))
    #verbatimTextOutput(ns('map_sample_table'))
  })
  
  observe({
    if (!is.null(input$mymap_marker_click$id)){
      callModule(module = resultsSampleFunc, id = "results_sample", input$mymap_marker_click$id)
    }
  })
}