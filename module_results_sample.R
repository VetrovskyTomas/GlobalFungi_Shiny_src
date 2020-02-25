# Function for module UI with SH
resultsSampleUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    fluidRow(
      column(4,tableOutput(ns('sample_table_basic'))),
      column(4,tableOutput(ns('sample_table_advance'))),
      column(4,tableOutput(ns('sample_table_paper')), 
             uiOutput(ns('dynamic_button')),
             br(),
             downloadButton(ns("downloadSeqs"), "Download sequences")
      )
    ),
    fluidRow(
      p("  *Please note that metadata were provided in the study or by the authors. Check Methods section of the paper for details on the used methodology."),
      p(" NA = not assessed")
    ),
    tags$head(tags$style(paste0("#",ns('sample_table_basic')," table {background-color: white; }"), media="screen", type="text/css")),
    tags$head(tags$style(paste0("#",ns('sample_table_advance')," table {background-color: white; }"), media="screen", type="text/css")),
    tags$head(tags$style(paste0("#",ns('sample_table_paper')," table {background-color: white; }"), media="screen", type="text/css"))
  )
}

# Function for module server logic
resultsSampleFunc <- function(input, output, session, id) {
  #namespace for dynamic input...
  ns <- session$ns  
  
  sample_id <- isolate(id)
    if (sample_id > -1){
    query <- sprintf(paste0("SELECT * FROM ",options()$mysql$samples," WHERE `id` = '",sample_id,"'"))
    sample <- data.table(sqlQuery(query))
    
    print(paste0("sample details ",sample_id))
    #print(sample)
    #print(sample[,"sample_name"])
    
    # table basic
    output$sample_table_basic <- renderTable({
      sample_vals <- sample[,c("id", "longitude", "latitude", "elevation", "MAT_study", "MAP_study", "continent", "country", "location",
                                      "sample_name", "sample_type", "Biome", "Biome_detail", "area_sampled", "area_GPS", "number_of_subsamples", "sample_depth",
                                      "year_of_sampling", "month_of_sampling", "day_of_sampling", "sampling_info", "sample_description")]
      colnames(sample_vals) <- c("Sample ID", "Longitude", "Latitude", "Elevation study (m)", "Mean annual temperature study (\u00B0C)", "Mean annual precipitation study (mm)","Continent", "Country", "Location",
                                 "Sample ID in study","Sample type","Biome (ENVO root)","Biome (ENVO)", "Area covered by sampling (m2)", "Area represented by GPS (m2)", "Number of subsamples", "Sampling depth (cm)",
                                 "Sampling year", "Sampling manth", "Sampling day", "Sampling info", "Sample description")
      sample_vals[sample_vals == "NA_"] <- "NA"
      sample_vals <- data.frame(variable = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Variable", " ")
      sample_vals
    })
    
    # table advance
    output$sample_table_advance <- renderTable({
      sample_vals <- sample[,c("sequencing_platform", "target_gene", "extraction_DNA_mass", "extraction_DNA_size", "extraction_DNA_method",
                                      "primers", "primers_sequence", "total_C_content", "total_N_content", "organic_matter_content",
                                      "pH", "pH_method", "total_Ca", "total_P", "total_K", "plants_dominant", "plants_all", "sample_info")]
      colnames(sample_vals) <- c("Sequencing platform", "Target marker", "Sample size for DNA extraction (g)", "Sample size for DNA extraction (other)", "DNA extraction method",
                                 "Primers", "Primer sequences", "C content (%)", "N content (%)", "Org. matter content (%)",
                                 "pH", "pH method", "Ca content (ppm)", "P total (ppm)", "K total (ppm)", "Dominant plant(s)", "Other plant(s)", "Additionel sample info")
      sample_vals[sample_vals == "NA_"] <- "NA"
      sample_vals <- data.frame(variable = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Variable", " ")
      sample_vals
    })
    
    # table paper
    output$sample_table_paper <- renderTable({
      sample_vals <- global_samples[which(global_samples$id %in% sample_id), ]
      sample_vals <- global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),c("title", "authors", "year","journal", "doi", "contact", "submitted_by")]
      colnames(sample_vals) <- c("Title", "Authors", "Year","Journal", "DOI", "Contact", "Submitted by")
      sample_vals <- data.frame(study = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Study", " ")
      sample_vals
    })
    
    output$dynamic_button <- renderUI({
      sample_vals <- global_samples[which(global_samples$id %in% sample_id), ]
      paper_id <- global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),c("paper_id")]
      actionButton(ns("showStudy"), "Show study samples", icon = icon("microscope"), onclick =paste0("window.open('/?paper=",paper_id,"', '_blank')"))
    })    
    
    output$downloadSeqs <- downloadHandler(
      filename = paste0(sample_id,"_sequences.zip"),
      content = function(file) {
        file.copy(paste0(global_samples_path,"sample_", sample_id ,".fa.gz"), file)
      }
    )
    }
}