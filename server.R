# Server
# test server push
function(input, output, session) {
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  
  observe({
    meta_table <- toTrainMetadata[,c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query[['Spec.Hyp']])) {
      if (input$study != "All" && length(input$study) > 0 ) {
        meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
      }
      updateSelectInput(session = session, 
                        inputId = "samp.name",
                        label = paste0("Samples: ", nrow(meta_table)),
                        choices = meta_table$sampleHascode
      )
    } else if (query[['Spec.Hyp']] %in% SHs) {
      meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
      
     #vec_of_SHs <- which(!is.na(match(SNV_counts$V4, query)))
     vec_of_SHs <- which(!is.na(match(SNV_counts$V3, query)))
      
      #selected_samples <- strsplit(x = SNV_counts$V2[vec_of_SHs], split = ";") %>%
      selected_samples <- strsplit(x = SNV_counts$V1[vec_of_SHs], split = ";") %>%
        unlist() %>%
        unique()
      
      selected_studies <- toTrainMetadata[sampleHascode %in% selected_samples]$paper_title_year %>%
        unique()
      
      updateSelectInput(session = session,
                        inputId = "study",
                        label = paste0("Studies: ", length(selected_studies)),
                        choices = c(selected_studies),
                        selected = c(selected_studies))
      
      # meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
      
    }
    meta_table <- toTrainMetadata[,c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    if (input$study != "All" && length(input$study) > 0 ) {
      meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    }
    updateSelectInput(session = session, 
                      inputId = "samp.name",
                      label = paste0("Samples: ", nrow(meta_table)),
                      choices = meta_table$sampleHascode
    )
  })
  
  # Filter data based on selections
  output$plot <- renderLeaflet({
    meta_table <- toTrainMetadata[,c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    if (input$study != "All" && length(input$study) > 0) {
      # paper <- input$paper
      # data <- toTrainMetadata[chmatch(x = input$study, table = paper_title_year), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
      meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    } 
    if (input$samp.name != "All" && length(input$samp.name) > 0) {
      # samp <- input$samp.name
      meta_table <- meta_table[chmatch(x = input$samp.name, table = sampleHascode), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
    }
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = meta_table, lng = ~longitude, lat = ~latitude, 
                 clusterOptions = markerClusterOptions(), 
                 layerId = meta_table$sampleHascode,
                 popup = meta_table$sampleHascode)
    
    
  })
  output$dis <- DT::renderDataTable({
    meta_table <- toTrainMetadata[,c("sampleHascode", "primers", "platform", "SH_Richness","paper_title_year", "paper_journal", "longitude", "latitude")]
    if (input$study != "All" && length(input$study) > 0) {
      # paper <- input$paper
      # data <- toTrainMetadata[chmatch(x = input$study, table = paper_title_year), c("sampleHascode", "longitude", "latitude", "paper_title_year")]
      meta_table <- meta_table[which(toTrainMetadata$paper_title_year %in% input$study), c("sampleHascode", "primers", "platform", "SH_Richness","paper_title_year", "paper_journal", "longitude", "latitude")]
    } 
    if (input$samp.name != "All" && length(input$samp.name) > 0) {
      # samp <- input$samp.name
      meta_table <- meta_table[chmatch(x = input$samp.name, table = sampleHascode), c("sampleHascode", "primers", "platform", "SH_Richness","paper_title_year", "paper_journal", "longitude", "latitude")]
    }
    meta_table
  })
  
  #session$onSessionEnded(stopApp) #This was giving problems to the url generation and usage. We need to discover why...
}
