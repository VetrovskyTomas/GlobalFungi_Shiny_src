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
    table_samples <- samples[,c("id", "primers", "longitude", "latitude", "sample_type", "ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    col_names <- c("ID", "primers", "longitude", "latitude", "type", "ITS tot.", "Biome", "MAT", "MAP", "pH", "year")
    if("abundances" %in% colnames(samples)) {
      table_samples <- samples[,c("id", "primers", "longitude", "latitude", "sample_type", "abundances", "ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
      col_names <- c("ID", "primers", "longitude", "latitude", "type", "ITS obs.", "ITS tot.", "Biome", "MAT", "MAP", "pH", "year")
    }
    
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