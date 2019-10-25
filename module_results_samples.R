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

  observe({
    
    table_samples <- variable$samples[,c("id", "primers", "longitude", "latitude", "sample_type", "ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    col_names <- c("ID", "primers", "longitude", "latitude", "type", "ITS tot.", "Biome", "MAT", "MAP", "pH", "year")
    if("abundances" %in% colnames(variable$samples)) {
      table_samples <- variable$samples[,c("id", "primers", "longitude", "latitude", "sample_type", "abundances", "ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
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
      write.table(variable$samples, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
  
  # sample details - clicked row...
  output$sample_info <- renderUI({
    req(length(input$metadata_cell_clicked) > 0)
    #input$metadata_rows_selected
    wellPanel(
      fluidRow(
        column(4,tableOutput(ns('sample_table_basic'))),
        column(4,tableOutput(ns('sample_table_advance'))),
        column(4,tableOutput(ns('sample_table_paper')), 
               downloadButton(ns("downloadSeqs"), "Download sequences")
        )
      ),
      fluidRow(
        "  *All metadata are provided by the outhors, please contact the authors for any questions."
      ),
      tags$head(tags$style(paste0("#",ns('sample_table_basic')," table {background-color: white; }"), media="screen", type="text/css")),
      tags$head(tags$style(paste0("#",ns('sample_table_advance')," table {background-color: white; }"), media="screen", type="text/css")),
      tags$head(tags$style(paste0("#",ns('sample_table_paper')," table {background-color: white; }"), media="screen", type="text/css"))
    )
  })
  
  #
  output$sample_table_basic <- renderTable({
    sample_vals <- variable$samples[input$metadata_rows_selected,]
    sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), 
                                  c("id", "longitude", "latitude", "elevation", "MAT_study", "MAP_study", 
                                    "country", "area_sampled", "number_of_subsamples", "sample_depth")]
    sample_vals <- data.frame(basic_metadata = rownames(t(sample_vals)), values = t(sample_vals))
  })
  #
  output$sample_table_advance <- renderTable({
    sample_vals <- variable$samples[input$metadata_rows_selected,]
    sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), 
                                  c("total_C_content", "total_N_content", "organic_matter_content", 
                                    "pH", "pH_method", "total_Ca", "total_P", "total_K", "Plants")]
    sample_vals <- data.frame(advance_metadata = rownames(t(sample_vals)), values = t(sample_vals))
  })
  #
  output$sample_table_paper <- renderTable({
    sample_vals <- variable$samples[input$metadata_rows_selected,]
    sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), ]
    sample_vals <- global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),c("title", "authors", "year","journal", "doi")]
    sample_vals <- data.frame(study = rownames(t(sample_vals)), values = t(sample_vals))
  })
}