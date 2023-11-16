# Function for module UI with SH
resutsSamplesUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(width = "100%", style = "background-color:white;",  
    fluidRow(
      column(6,downloadButton(ns("downloadData"), "Download metadata"))
    ),
    fluidRow(
      br(),
      DT::dataTableOutput(ns("metadata")),
      br(),
      uiOutput(ns("sample_info"))
    )
  )
}

# Function for module server logic
resutsSamplesFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns
  
  samples <- isolate(variable$samples)

  observe({
    print("SAMPLES REFRESH...")
    
    #table_samples <- samples[,c("permanent_id", "sample_type", "Biome", "longitude", "latitude", "MAT", "MAP", "pH", "year_of_sampling", "primers", "ITS_total", "manipulated")]
    
    #table_samples <- samples[,c("permanent_id", "sample_type", "Biome", "longitude", "latitude", "MAT", "MAP", "pH", "year_of_sampling_from", "year_of_sampling_to", "primers", "ITS_total", "manipulated")]
    #table_samples$year_of_sampling <- ifelse(table_samples$year_of_sampling_from == table_samples$year_of_sampling_to,
    #                                            as.character(table_samples$year_of_sampling_from),
    #                                            paste(table_samples$year_of_sampling_from, table_samples$year_of_sampling_to, sep = "-"))
    #col_names <- c("Sample ID", "Sample type", "Biome", "Longitude", "Latitude", "MAT (\u00B0C) CHELSA", "MAP (mm) CHELSA", "pH", "Sampling year", "Primers", "ITS total", "Manipulated")
    #if("abundances" %in% colnames(samples)) {
    #  table_samples <- samples[,c("permanent_id", "sample_type", "Biome", "longitude", "latitude", "MAT", "MAP", "pH", "year_of_sampling_from", "year_of_sampling_to", "primers", "abundances", "ITS_total", "manipulated")]
    #  table_samples$year_of_sampling <- ifelse(table_samples$year_of_sampling_from == table_samples$year_of_sampling_to,
    #                                           as.character(table_samples$year_of_sampling_from),
    #                                           paste(table_samples$year_of_sampling_from, table_samples$year_of_sampling_to, sep = "-"))
    #  col_names <- c("Sample ID", "Sample type", "Biome", "Longitude", "Latitude", "MAT (\u00B0C)", "MAP (mm)", "pH", "Sampling year", "Primers","ITS observed", "ITS total", "Manipulated")
    #} 
    
    
    #####################
    table_samples <- samples[,c("permanent_id", "sample_type", "Biome", "longitude", "latitude", "MAT", "MAP", "pH")]
    col_names <- c("Sample ID", "Sample type", "Biome", "Longitude", "Latitude", "MAT (\u00B0C)", "MAP (mm)", "pH")
    
    if("abundances" %in% colnames(samples)) {
      table_samples$year_of_sampling <- ifelse(samples$year_of_sampling_from == samples$year_of_sampling_to,
                                               as.character(samples$year_of_sampling_from),
                                               paste(samples$year_of_sampling_from, samples$year_of_sampling_to, sep = "-"))
      table_samples$primers <- samples$primers
      table_samples$abundances <- samples$abundances
      table_samples$ITS_total <- samples$ITS_total
      table_samples$manipulated = ifelse(samples$manipulated == 1, "Yes", "No")
      col_names <- c(col_names, "Sampling year", "Primers","ITS observed", "ITS total", "Manipulated")
    } else {
      table_samples$year_of_sampling <- ifelse(samples$year_of_sampling_from == samples$year_of_sampling_to,
                                               as.character(samples$year_of_sampling_from),
                                               paste(samples$year_of_sampling_from, samples$year_of_sampling_to, sep = "-"))
      table_samples$primers <- samples$primers
      table_samples$ITS_total <- samples$ITS_total
      table_samples$manipulated = ifelse(samples$manipulated == 1, "Yes", "No")
      col_names <- c(col_names, "Sampling year", "Primers", "ITS total", "Manipulated")
    }
    
    
    #####################
    
    # table with samples metadata...
    output$metadata <- DT::renderDataTable({
      table_samples
    }, colnames = col_names, selection = 'single')
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "sample_list.txt",
    content = function(file) {
      write.table(samples, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
  

  # show additional
  observeEvent(input$metadata_cell_clicked,{
    s_id <- reactiveVal()
    s_id <- samples[input$metadata_rows_selected,]$id[[1]]
    if (!is.null(s_id)){
      output$sample_info <- renderUI({
        req(length(input$metadata_cell_clicked) > 0)
        callModule(module = resultsSampleFunc, id = "results_sample", isolate(s_id))
        resultsSampleUI(id = ns("results_sample"))
      })
    }
  }, ignoreInit = TRUE)
}