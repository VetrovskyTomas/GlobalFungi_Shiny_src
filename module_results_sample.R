# Function for module UI with SH
resultsSampleUI <- function(id) {
  ns <- NS(id)
  
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
}

# Function for module server logic
resultsSampleFunc <- function(input, output, session, sample_id) {

    query <- sprintf(paste0("SELECT * FROM ",options()$mysql$samples," WHERE `id` = '",sample_id,"'"))
    sample <- data.table(sqlQuery(query))
    
    print(paste0("sample details ",sample_id))
    
    #namespace for dynamic input...
    ns <- session$ns  
    
    # table basic
    output$sample_table_basic <- renderTable({
      sample_vals <- sample[which(sample$id %in% sample_id), 
                                    c("id", "longitude", "latitude", "elevation", "MAT_study", "MAP_study", "continent", "country",
                                      "sample_name", "sample_type", "Biome", "Biome_detail", "area_sampled", "number_of_subsamples", "sample_depth",
                                      "year_of_sampling", "month_of_sampling", "day_of_sampling", "sampling_info", "sample_description")]
      sample_vals <- data.frame(basic_metadata = rownames(t(sample_vals)), values = t(sample_vals))
    })
    
    # table advance
    output$sample_table_advance <- renderTable({
      sample_vals <- sample[which(sample$id %in% sample_id), 
                                    c("sequencing_platform", "target_gene", "extraction_DNA_mass", "extraction_DNA_size", "extraction_DNA_method",
                                      "primers", "primers_sequence", "sample_seqid", "sample_barcode", "total_C_content", "total_N_content", "organic_matter_content",
                                      "pH", "pH_method", "total_Ca", "total_P", "total_K", "plants_dominant", "plants_all", "sample_info")]
      
      #colnames(sample_vals) <- c("a1", "a2", "a3", 
      #              "pH", "pH_method", "total_Ca", "total_P", "total_K", "plants_dominant")
      
      sample_vals <- data.frame(advance_metadata = rownames(t(sample_vals)), values = t(sample_vals))
    })
    
    # table paper
    output$sample_table_paper <- renderTable({
      sample_vals <- global_samples[which(global_samples$id %in% sample_id), ]
      sample_vals <- global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),c("title", "authors", "year","journal", "doi", "contact")]
      sample_vals <- data.frame(study = rownames(t(sample_vals)), values = t(sample_vals))
    })
}