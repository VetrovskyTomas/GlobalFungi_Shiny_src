# Function for module UI
insertMetadataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6, 
        br(),
        h2(id="section_title", "1) Download and fill template table (don't change the headers)"),
        br(),
        downloadButton(ns("buttTemplate"), label = "Download metadata template (xlsx)")
      ),
      column(6,
        br(),
        h2(id="section_title", "2) Insert filled template table (xlsx file)"),
        fileInput(ns('fileXLSX'), '',accept = c(".xlsx")),
      )
    ),
    fluidRow(
      column(6, 
        h2(id="section_title", "3) Click below to validate your submission"),
        br(),
        actionButton(ns("buttStart"), label = "Process metadata", icon = icon("microchip")))
    ),
    fluidRow(
      column(12, br(), verbatimTextOutput(ns('info_errors')))
    )
  )
}

# server logic to read selected file ----
insertMetadataFunc <- function(input, output, session, study) {
  
  ns <- session$ns
  
  metadata <- reactiveValues()
  metadata$datapath <- NULL
  
  output$buttTemplate <- downloadHandler(
    filename = "TemplateMetadata.xlsx",
    content = function(file) {
    file.copy(paste0("TemplateMetadata.xlsx"), file)
  })
  
  observe({
    if (is.null(input$fileXLSX$datapath)){
      shinyjs::disable("buttStart")
    } else {
      shinyjs::enable("buttStart")
      metadata$datapath <- input$fileXLSX$datapath
    }
  })
  
  observeEvent(input$buttStart, {
    if (!is.null(metadata$datapath)){
    metatable <- as.data.frame(read_excel(metadata$datapath, 1, col_names = FALSE))
    if (nrow(metatable)>1){
      info = ""
      print(paste0("Table contans ", (length(metatable[,1])-1), " sample rows."))

      # NON-OPTIONAL
      # 1	2	4	7	8	12	17	21	22	23	27	40
      nonoptional_vals <- c(1,2,4,7,8,12,17,21,22,23,27,40)
      # MANDATORY
      # 1	2	12	17	21	27	40
      mandatory_vals <- c(1,2,12,17,21,27,40)
      # SELECT
      # 4	7	8	22	23
      select_vals <- c(4,7,8,22,23)
      
      for (x in c(1:ncol(metatable))) {
        #print(colnames(metatable[x]))
        #print(length(metatable[,x]))
        sel_list <- NULL
        sel <- ""
        if (x %in% select_vals){
          str <- metatable[1,x]
          #print(str)
          sel <- substr(str, str_locate(str, ": ")+2, nchar(str))
          sel_list <- unlist(strsplit(sel, "[/]")) 
          #print(sel_list)
        }
        #############
        if (x %in% nonoptional_vals){
          print(paste0("**** Show column: ", colnames(metatable[x])," ********"))
          for (y in c(3:nrow(metatable))) {
            #print(metatable[y,x])
            if (!is.na(metatable[y,x])){
              if (x %in% select_vals){
                if (!(toString(metatable[y,x]) %in% sel_list)) {
                  info = paste0(info, "VALUE NOT SELECTED in column [",colnames(metatable[x]),"] (row ",y,") - current value: ",metatable[y,x],"\n Options: ",sel,"\n")
                }
              } else {
                if (toString(metatable[y,x]) == ""){
                  info = paste0(info, "EMPTY VALUE in mandatory column [",colnames(metatable[x]),"] (row ",y,") - current value: ",metatable[y,x],"\n")
                }
              }
            } else {
              info = paste0(info, "MISSING VALUE in mandatory column [",colnames(metatable[x]),"] (row ",y,") - current value: ",metatable[y,x],"\n")
            }
              
          }
        }
      }
      
      #info = paste0(info,"stop")

      if (info != "") {
        info = paste0("There are some errors - please correct your file and upload it again: \n\n",info)
        
        alert(paste0(info))
        study$info <- info
        
        output$info_errors <- renderText(
          info
        )
      } else {
        print("You processed the metadata...")
        study$info <- "You processed the metadata..."
        
        study$metadata$num_of_samples <- nrow(metatable)-2
        study$metadata$data <- metatable
        
        info <- ""
        # DETECT PREVIOUS DATA AND DELETE THEM...
        query <- sprintf(paste0("SELECT COUNT(*) from ",options()$mysql$metadata," WHERE `paper_study` = '",study$key,"'"))
        meta_count <- sqlQuery(query)
        if (meta_count != "0"){
          info <- paste0("There are already metadata connected to the study (",meta_count," records) - old metdata were deleted...")
          sqlQuery(sprintf(paste0("DELETE from ",options()$mysql$metadata," WHERE `paper_study` = '",study$key,"'")))
        }
        print(info)
        
        #INSERT THE METADATA...
        withProgress(message = 'Saving metadata', {
        
          for (x in c(3:nrow(metatable))) {
            incProgress(1/(nrow(metatable)-1))

            query <- paste0("INSERT INTO ",options()$mysql$metadata,
                            " (paper_study,latitude,longitude,elevation,continent,country,location,sample_type,Biome,Biome_detail,MAT_study,MAP_study,sample_name,area_sampled,area_GPS,number_of_subsamples,sample_depth,year_of_sampling,month_of_sampling,day_of_sampling,sampling_info,sample_description,sequencing_platform,target_gene,extraction_DNA_mass,extraction_DNA_size,extraction_DNA_method,primers,primers_sequence,pH,pH_method,organic_matter_content,total_C_content,total_N_content,total_P,total_Ca,total_K,plants_dominant,plants_all,sample_info,sample_seqid,sample_barcode) VALUES ('",
                            study$key, "', '",
                            safeSqlQueryVal(metatable[x,1]), "', '",
                            safeSqlQueryVal(metatable[x,2]), "', '",
                            safeSqlQueryVal(metatable[x,3]), "', '",
                            safeSqlQueryVal(metatable[x,4]), "', '",
                            safeSqlQueryVal(metatable[x,5]), "', '",
                            safeSqlQueryVal(metatable[x,6]), "', '",
                            safeSqlQueryVal(metatable[x,7]), "', '",
                            safeSqlQueryVal(metatable[x,8]), "', '",
                            safeSqlQueryVal(metatable[x,9]), "', '",
                            safeSqlQueryVal(metatable[x,10]), "', '",
                            safeSqlQueryVal(metatable[x,11]), "', '",
                            safeSqlQueryVal(metatable[x,12]), "', '",
                            safeSqlQueryVal(metatable[x,13]), "', '",
                            safeSqlQueryVal(metatable[x,14]), "', '",
                            safeSqlQueryVal(metatable[x,15]), "', '",
                            safeSqlQueryVal(metatable[x,16]), "', '",
                            safeSqlQueryVal(metatable[x,17]), "', '",
                            safeSqlQueryVal(metatable[x,18]), "', '",
                            safeSqlQueryVal(metatable[x,19]), "', '",
                            safeSqlQueryVal(metatable[x,20]), "', '",
                            safeSqlQueryVal(metatable[x,21]), "', '",
                            safeSqlQueryVal(metatable[x,22]), "', '",
                            safeSqlQueryVal(metatable[x,23]), "', '",
                            safeSqlQueryVal(metatable[x,24]), "', '",
                            safeSqlQueryVal(metatable[x,25]), "', '",
                            safeSqlQueryVal(metatable[x,26]), "', '",
                            safeSqlQueryVal(metatable[x,27]), "', '",
                            safeSqlQueryVal(metatable[x,28]), "', '",
                            safeSqlQueryVal(metatable[x,29]), "', '",
                            safeSqlQueryVal(metatable[x,30]), "', '",
                            safeSqlQueryVal(metatable[x,31]), "', '",
                            safeSqlQueryVal(metatable[x,32]), "', '",
                            safeSqlQueryVal(metatable[x,33]), "', '",
                            safeSqlQueryVal(metatable[x,34]), "', '",
                            safeSqlQueryVal(metatable[x,35]), "', '",
                            safeSqlQueryVal(metatable[x,36]), "', '",
                            safeSqlQueryVal(metatable[x,37]), "', '",
                            safeSqlQueryVal(metatable[x,38]), "', '",
                            safeSqlQueryVal(metatable[x,39]), "', '",
                            safeSqlQueryVal(metatable[x,40]), "', '",
                            safeSqlQueryVal(metatable[x,41]), "')")
            sqlQuery(query)
          }
          
        })
        
        alert(paste0(info,"\n","You have inserted your sample metadata successfully!"))
        metadata$inserted <- TRUE
        #
        
        study$metadata$test <- "OK"
        
        output$info_errors <- renderText(
          study$metadata$test
        )
      }
    } else {
      alert(paste0("Error: Table is empty!"))
    }
    
    #
    shinyjs::disable("buttStart")
    }
  })
}

