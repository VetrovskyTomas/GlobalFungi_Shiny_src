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
      result_basic <- global_samples[id == sample_id,]
      
      # id  44884
      # paper 505
      # permanent_id  GF3S02027a
      # sample_type root
      # latitude  18.586694
      # longitude -95.074407
      # continent North America
      # year_of_sampling_from 2014
      # year_of_sampling_to 2014
      # biome forest
      # primers ITS1F_KYO1/ITS2_KYO2
      # MAT 24.9
      # MAP 3079.0
      # pH  NULL
      # SOC NULL
      # ITS_total 4540
      # manipulated True
      
      #print(result_basic)
      permanent_id <- result_basic[,"permanent_id"]
      #print(paste0("sample details ",permanent_id))
      #print(paste0("sample_id ",sample_id))
    query <- sprintf(paste0("SELECT * FROM ",options()$mysql$advanced," WHERE `id` = ",sample_id,""))
    result_advanced <- data.table(sqlQuery(query))
    
    sample <- cbind(result_basic, result_advanced)
    #print(sample)
    #print(sample[,"sample_name"])
    
    # table basic
    output$sample_table_basic <- renderTable({
      sample_vals <- sample[,c("permanent_id", "longitude", "latitude", "elevation", 
                               "continent",
                               "country", 
                               "location",
                               "sample_type", 
                               "Biome", 
                               "Biome_detail", 
                               "MAT_study", 
                               "MAP_study",
                               "sample_name",
                               "area_sampled")]
      
      sample_vals[, area_GPS :=  ifelse(sample$area_GPS_from == sample$area_GPS_to,
                                                    as.character(sample$area_GPS_from),
                                                    paste(sample$area_GPS_from, "-", sample$area_GPS_to))]
      
      sample_vals[, number_of_subsamples := ifelse(sample$number_of_subsamples_from == sample$number_of_subsamples_to,
                                     as.character(sample_vals$number_of_subsamples_from),
                                     paste(sample_vals$number_of_subsamples_from, "-", sample_vals$number_of_subsamples_to))]
      
      sample_vals[, sample_depth := ifelse(sample$sample_depth_from == sample$sample_depth_to,
                                     as.character(sample$sample_depth_from),
                                     paste(sample$sample_depth_from, "-", sample$sample_depth_to))]
      
      sample_vals[, year_of_sampling := ifelse(sample$year_of_sampling_from == sample$year_of_sampling_to,
                                               as.character(sample$year_of_sampling_from),
                                               paste(sample$year_of_sampling_from, "-", sample$year_of_sampling_to))]
      
      sample_vals[, c("month_of_sampling", "day_of_sampling", "sampling_info", "sample_description", "manipulated") :=
                    sample[, .(month_of_sampling, day_of_sampling, sampling_info, sample_description, manipulated)]]
    
      
      # Remove the columns "area_GPS_from" and "area_GPS_to" from the data frame
      #sample_vals <- sample_vals[, !names(sample_vals) %in% c("area_GPS_from", "area_GPS_to")]
      
      colnames(sample_vals) <- c("Sample ID", "Longitude", "Latitude", "Elevation study (m)", 
                                 "Continent", 
                                 "Country", 
                                 "Location",
                                 "Sample type",
                                 "Biome",
                                 "Biome (ENVO)",
                                 "MAT (\u00B0C) study", 
                                 "MAP (mm) study",
                                 "Sample ID in study",
                                 "Area covered by sampling (m2)",
                                 "Area represented by GPS (m2)", 
                                 "Number of subsamples",  
                                 "Sampling depth (cm)",
                                 "Sampling year", "Sampling month", "Sampling day", "Sampling info", "Sample description", "Manipulated")
      sample_vals[sample_vals == "NA_"] <- "NA"
      sample_vals <- data.frame(variable = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Variable", " ")
      sample_vals
    })
    
    # table advance
    output$sample_table_advance <- renderTable({
      sample_vals <- sample[,c("sequencing_platform", "target_gene")]
      
      sample_vals[, extraction_DNA_mass := ifelse(sample$extraction_DNA_mass_from == sample$extraction_DNA_mass_to,
                                           as.character(sample$extraction_DNA_mass_from),
                                           paste(sample$extraction_DNA_mass_from, "-", sample$extraction_DNA_mass_to))]
      
      sample_vals[, c("extraction_DNA_size", "extraction_DNA_method",
                      "primers", "primers_sequence", 
                      "pH", 
                      "pH_method", 
                      "organic_matter_content",
                      "total_C_content", 
                      "total_N_content",
                      "total_P", 
                      "total_Ca", 
                      "total_K", 
                      "plants_dominant", 
                      "plants_all", "sample_info") :=
                    sample[, .(extraction_DNA_size, extraction_DNA_method,
                               primers, primers_sequence, 
                               pH, 
                               pH_method, 
                               organic_matter_content,
                               total_C_content, 
                               total_N_content,
                               total_P, 
                               total_Ca, 
                               total_K, 
                               plants_dominant, 
                               plants_all, sample_info)]]
      
      colnames(sample_vals) <- c("Sequencing platform", "Target marker", "Sample mass for DNA extraction (g)", "Sample size for DNA extraction (other)", "DNA extraction method",
                                "Primers", "Primer sequences", 
                                "pH", 
                                "pH method",
                                "Org. matter content (%)",
                                "Organic C content (%)", 
                                "N content (%)", 
                                "Total P content (ppm)",
                                "Ca content (ppm)",
                                "K content (ppm)", 
                                "Dominant plant(s)", "Other plants", "Additional sample info")
      sample_vals[sample_vals == "NA_"] <- "NA"
      sample_vals <- data.frame(variable = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Variable", " ")
      sample_vals
    })
    
    # table paper
    output$sample_table_paper <- renderTable({
      sample_vals <- global_samples[which(global_samples$id %in% sample_id), ]
      sample_vals <- global_papers[which(global_papers$id %in% sample_vals$paper),c("title", "authors", "year","journal", "doi", "contact")]
      sample_vals$submitted_by <- global_info$name
      colnames(sample_vals) <- c("Title", "Authors", "Year","Journal", "DOI", "Contact", "Submitted by")
      sample_vals <- data.frame(study = rownames(t(sample_vals)), values = t(sample_vals))
      names(sample_vals) <- c("Study", " ")
      sample_vals
    })
    
    output$dynamic_button <- renderUI({
      sample_vals <- global_samples[which(global_samples$id %in% sample_id), ]
      paper_id <- global_papers[which(global_papers$id %in% sample_vals$paper),c("id")]
      actionButton(ns("showStudy"), "Show study samples", icon = icon("microscope"), onclick =paste0("window.open('/?paper=",paper_id,"', '_blank')"))
    })    
    
    output$downloadSeqs <- downloadHandler(
      filename = paste0(permanent_id,"_sequences.zip"),
      content = function(file) {
        file.copy(paste0(global_samples_path, permanent_id,"_sample.zip"), file)
      }
    )
    }
}