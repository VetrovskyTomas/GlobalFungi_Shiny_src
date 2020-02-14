# Function for module UI
geoshapeUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  fluidPage(
    useShinyjs(),
    h1(id="welcome_title", "Geosearch!"),
    sidebarPanel(width = "100%", style = "background-color:white;",
      fluidRow(
        leafletOutput(ns("mymap"), height=500)
      ),
      br(),
      fluidRow(
        column(2,actionButton(ns("buttonSH"), "Analize SH", icon =icon("sitemap"))),
        column(10,verbatimTextOutput(ns('info_selected_samples')))
      ),
        sidebarPanel(width = "100%", style = "background-color:white;",
        tabsetPanel(id = ns("tabs"),
          tabPanel("SH selection ", value = "tab_selSH",
            wellPanel(style = "background-color:white;",  
              fluidRow(
                column(6,downloadButton(ns("downloadSHs"), "Download SH list"))
              ),
              fluidRow(
                br(),
                DT::dataTableOutput(ns("selection_SH_out"))
              )
            )
          ),
          tabPanel("Selected samples", value = "tab_selSamples",
            wellPanel(style = "background-color:white;",  
              fluidRow(
                column(6,downloadButton(ns("downloadData"), "Download metadata"))
              ),
              fluidRow(
                br(),
                DT::dataTableOutput(ns("selection_samples_out"))
              )
            )
          )
        )
      )
    )
  )
}

# Function for module server logic
geoshapeFunc <- function(input, output, session, samples) {

  #namespace for dynamic input...
  ns <- session$ns
  
  selected_global <- reactiveValues()
  selected_global$samples <- NULL

  # define map samples...
  map_samples <- data.frame(locationID = global_samples$id,
                    longitude = global_samples$longitude,
                    latitude = global_samples$latitude,
                    stringsAsFactors = F)
  
  map_samples$secondLocationID <- paste(as.character(map_samples$locationID), "_selectedLayer", sep="")
  coordinates <- SpatialPointsDataFrame(map_samples[,c('longitude', 'latitude')] , map_samples)
  #print(coordinates)
  proxy <- leafletProxy(mapId = "mymap")
  ################################################# section one #################################################
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  ################################################# section two #################################################
  
  # base map
  output$mymap <- renderLeaflet({
    leaflet(data = map_samples, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
      addProviderTiles(providers$Esri, options = providerTileOptions(
        updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
        updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
      ) %>%
      addCircleMarkers(data = map_samples, ~longitude, ~latitude, options = pathOptions(clickable = FALSE),
                       color = "black",
                       radius = 4,
                       fillColor = "red",
                       weight = 1,
                       stroke = T,
                       fillOpacity = 1 
      ) %>%
    addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            circleMarkerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'black'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'black'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'black'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })
  
  ############################################### section three #################################################
  observeEvent(input$mymap_draw_new_feature,{
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                     , location_coordinates = coordinates
                                     , location_id_colname = "locationID")
    
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    # look up airports by ids found
    selected <- subset(map_samples, locationID %in% data_of_click$clickedMarker)
    # data_of_click_persistent$clickedMarker <- append(data_of_click$clickedMarker[data_of_click$clickedMarker %in%  data_of_click_persistent$clickedMarker],
    #                                                  data_of_click_persistent$clickedMarker)
    
    ######################################################
    selected_global$samples <- subset(global_samples, id %in% selected$locationID)
    # hide results when changed selection...
    shinyjs::hide(id = "tabs")
    hideTab(inputId = "tabs", target = "tab_selSH")
    hideTab(inputId = "tabs", target = "tab_selSamples")
    # get info...
    output$info_selected_samples <- renderText({
      num_samples <- 0
      if (!is.null(selected_global$samples)){
        num_samples <- nrow(selected_global$samples)
      } 
      return(paste0("You have selected ", num_samples, " samples."  ))
    })
    ######################################################
    #print(as.character(selected$secondLocationID))
    proxy %>% addCircleMarkers(data = selected,
        color = "gray", 
        radius = 4,
        lat = selected$latitude,
        lng = selected$longitude,
        fillColor = "green",
        fillOpacity = 1,
        weight = 1,
        stroke = T,
        layerId = as.character(selected$secondLocationID))
  })
  ############################################### section four ##################################################
  observeEvent(input$mymap_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$mymap_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = coordinates
                                         , location_id_colname = "secondLocationID")
      
      #print(bounded_layer_ids)
      # remove second layer representing selected locations
      proxy %>% removeMarker(as.character(bounded_layer_ids)) 
      
      first_layer_ids <- subset(map_samples, secondLocationID %in% bounded_layer_ids)$locationID
      #print(first_layer_ids)
      ######################################################
      #print(selected_global$samples)
      selected_global$samples <- subset(selected_global$samples, !selected_global$samples$id %in% first_layer_ids)
      ######################################################
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker %in% first_layer_ids]
    }
    ######################################################
    # hide results when changed selection...
    shinyjs::hide(id = "tabs")
    hideTab(inputId = "tabs", target = "tab_selSH")
    hideTab(inputId = "tabs", target = "tab_selSamples")
    # get info...
    output$info_selected_samples <- renderText({
      num_samples <- 0
      if (!is.null(selected_global$samples)){
        num_samples <- nrow(selected_global$samples)
      } 
      return(paste0("You have selected ", num_samples, " samples."  ))
    })
    ######################################################
  })
  
  #observe(print(reactiveValuesToList(input)))
  observeEvent(input$buttonSH,{
    print("Get SH in selected samples...")
    num_samples <- 0
    if (!is.null(selected_global$samples)){
      num_samples <- nrow(selected_global$samples)
    }
    if (num_samples > 0){
      shinyjs::show(id = "tabs")
      showTab(inputId = "tabs", target = "tab_selSH")
      showTab(inputId = "tabs", target = "tab_selSamples")
      # compute SH lists...
      withProgress(message = 'Searching...', {
        incProgress(1/3, detail = "getting SH names...")
        
        key_string <- paste0("('",paste(selected_global$samples$id, collapse="','" ),"')")
        query <- paste0("SELECT * from ",options()$mysql$samples_to_sh_table," WHERE `sample` IN ",key_string)
        result <- sqlQuery(query)
        
        SH_matches <- strsplit(result$SHs, ';', fixed=TRUE)
        
        # continue with list of SHs...
        SH_matches <- unique(unlist(SH_matches))
        print(paste0("Number of SH in ",length(selected_global$samples$id)," samples is ", length(SH_matches)))
        
        selected_global$sel_SH <- global_SH[which(global_SH$SH_id %in% SH_matches),]
      })
      
      #
      output$info_selected_samples <- renderText({
        return(paste0("You have analyzed ", nrow(selected_global$samples), " samples. (covering ",nrow(selected_global$sel_SH)," SH)"  ))
      })
      # table with samples metadata...
      output$selection_samples_out <- DT::renderDataTable({
        selected_global$samples[,c("id", "primers", "longitude", "latitude", "sample_type", "ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
      }, colnames = c("ID", "primers", "longitude", "latitude", "type", "ITS tot.", "Biome", "MAT", "MAP", "pH", "year"), selection = 'single')
      # table with SH from selection...
      output$selection_SH_out <- DT::renderDataTable(
        DT::datatable({
          sel_SH <- selected_global$sel_SH[,c("SH","Kingdom","Phylum","Class","Order","Family","Genus","Species")] 
          sel_SH <- sel_SH %>% mutate(SH = paste0("<a href='", "/?SH=",SH,"' target='_blank'>", SH,"</a>"))
          #sel_SH <- sel_SH %>% mutate(Species = ifelse(!grepl(" sp.", Species), paste0("<a href='", "/?species=",Species,"' target='_blank'>", Species,"</a>"),Species))
          sel_SH <- sel_SH %>% mutate(Species = ifelse(!grepl(" sp.", Species), ifelse(!grepl("unidentified", Species), paste0("<a href='", "/?species=",Species,"' target='_blank'>", Species,"</a>"),Species),Species))
          sel_SH
        }, escape = FALSE, selection = 'none')
      )
      # table with SH outside selection...
      output$selection_SH_out_compl <- DT::renderDataTable({
        NULL
      }, selection = 'single')
    }
  })
  
  # Downloadable csv of selected SHs...
  output$downloadSHs <- downloadHandler(
    filename = "sh_list.txt",
    content = function(file) {
      write.table(selected_global$sel_SH, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "sample_list.txt",
    content = function(file) {
      write.table(selected_global$samples, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
  
  observe({ 
    if (is.null(selected_global$samples)){
      shinyjs::hide(id = "tabs")
      hideTab(inputId = "tabs", target = "tab_selSH")
      hideTab(inputId = "tabs", target = "tab_selSamples")
    }
  })
  
}