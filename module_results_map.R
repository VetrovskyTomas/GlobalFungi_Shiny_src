# Function for module UI with SH
resutsMapUI <- function(id) {
  ns <- NS(id)
  #world map...
  sidebarPanel(width = "100%", style = "background-color:white;", 
    fluidRow(
      leafletOutput(ns("mymap")),
      uiOutput(ns("map_sample_info"))
    )
  )
}

# Function for module server logic
resutsMapFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  # map of sample from the study...
  output$mymap <- renderLeaflet({
    leaflet(data = variable$samples, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
      addProviderTiles(providers$Esri, options = providerTileOptions(
        updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
        updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
      ) %>%
      addCircleMarkers(data = variable$samples, ~longitude, ~latitude, layerId = ~id,
                       color = "black",
                       radius = 4,
                       fillColor = "red",
                       weight = 1,
                       stroke = T,
                       fillOpacity = 1#,
      ) 
  })
  
  # pop up text when clicked on map...
  observe({
    click <- input$mymap_marker_click
    if (is.null(click)) {
      print("null click")
      return()
    }
    #print(click)
    text <- paste0(input$mymap_marker_click$id, " ...number of seqs for TAXON / ALL ")
    leafletProxy(mapId = "mymap") %>%
      clearPopups() %>%
      addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
  })
  
  # map sample details - clicked row...
  output$map_sample_info <- renderUI({
    req(length(input$mymap_marker_click) > 0)
    verbatimTextOutput(ns('map_sample_table'))
  })
  
  #
  output$map_sample_table <- renderText({
    sample_vals <- global_samples[which(global_samples$id %in% input$mymap_marker_click$id),]
    #paste0(toString(sample_vals$id)," ",toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
    paste(input$mymap_marker_click$id,toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
  })
}