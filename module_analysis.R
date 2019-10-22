# Function for module UI
analysisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "Analyze sequences!"),
    
      # Sidebar panel for inputs ----
      sidebarPanel(width = 12,
        
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste you sequence", 
          "ATGAATCAGCACCGTGTTTTTGTTAATGCGGAGGCTGGACTTGGATGTTGCCGTCGTTGGCTCGTCCTTAAATGCATTAGCCTGGCGCGGCCAAGGCCTCTTTGGTGTGATAATTATCTACGCTGCTGGGGTTTGTGCTTTGTTGCTGGCTTACAGTCGTCCTCGGACAACTTTTTTTG", 
          width="100%",
          height = "200px"),
        
        # Input: Select a file ----
        # fileInput("file1", "Choose FASTA file",
        # multiple = FALSE,
        # accept = c("text/csv",
        # "text/comma-separated-values,text/plain",
        # ".csv")),
    
        hr(),
        
        #selectors
        fluidRow(
          column(4, checkboxInput(ns("is_extracted"), "ITS is already extracted", TRUE)),
          column(8, uiOutput(ns('dynamicInput_ITS')))
        ),
        fluidRow(
          column(8,selectInput(ns("search_type"), "Type:", choices = c("BLAST", "Exact")))
        ),
        hr(),
        fluidRow(
          column(6,actionButton(ns("analyze_button"), label = "Analyze"))
        ),
        hr(),
        fluidRow(
          column(12,DT::dataTableOutput(ns('info_table')))
        )
      )
    
  )
}

# Function for module server logic
analysisFunc <- function(input, output, session, parent) {
  
  #namespace for dynamic input...
  ns <- session$ns
  
  # to be shared...
  vals <- reactiveValues()
  vals$seq_hash <- NULL
  
  # When user clicks on submit button : Update result tab...
  observeEvent(input$analyze_button, {
    if (input$search_type == "Exact"){
      print("Searching for exact sequence match...")
      vals$type =  "sequence"
      vals$text <- input$textSeq
      vals$key <- md5_hash <- as.character(digest(input$textSeq, algo="md5", serialize=F))
      callModule(session = parent, module = resultsFunc, id = "id_results",vals, parent = parent)
      updateTabItems(session = parent, "menu_tabs", "fmd_results")
    } else {
      print(print(paste0("Run the blast... exists global_blast_out: ",exists("global_blast_out"))))
      if (!exists("global_blast_out")){
        withProgress(message = 'Running BLAST...', {
          incProgress(1/5)
          # generate folder for user task...
          outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
          system(paste("mkdir ", outputDir, sep = "")) #for linux
          incProgress(1/5)
          # write input fasta...
          input_fasta <- data.frame(seq1 = c(">query_seq", input$textSeq), stringsAsFactors = F)
          write.table(x = input_fasta,
                      file = paste(outputDir,"my_query.fasta", sep = ""),
                      quote = F, col.names = F, row.names = F)
          incProgress(1/5)
          # run blast command...
          cmd_blast <- paste0("blastn -db /home/fungal/databases/blast_database/fm_sequences_vol1.fa -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out -outfmt 6")
          system(cmd_blast)
          incProgress(1/5)
          # Check if your blast finished ## please lets improve this to something more cleaver ##
          if(!file.exists(paste0(outputDir, "results.out"))){
              blast_out <- NULL
          } else {
              blast_out <- read.delim(file = paste0(outputDir, "results.out"), header = F)
              # remove folder after use...
              cmd_blast <- paste0("rm -rf ",outputDir)
              system(cmd_blast)
          }
          incProgress(1/5)
        })
      } else {
        withProgress(message = 'Loading...', {
          for(i in 1:5) {
            incProgress(1/5)
            Sys.sleep(0.1)
          }
        })
          blast_out <- global_blast_out
      }
      
      # prepare output...
      if (!is.null(blast_out)){
      # add buttons...
      shinyInput <- function(FUN, len, id, label, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
          inputs[i] <- as.character(FUN(paste0(id, i), label[i], ...))
        }
        inputs
      }      
      # modify output...
      if (!is.null(blast_out)){
      names(blast_out) <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
      vals$seq_hash <- blast_out[,c("sseqid","pident")]
      blast_out$hits <- shinyInput(actionButton, nrow(blast_out), 'button_', label = blast_out[,"sseqid"], 
                                      onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                       "Shiny.onInputChange('", ns("lastClick"), "', Math.random())"))
      blast_out <- blast_out[,c("hits", "pident", "qstart", "qend", "sstart", "send", "evalue", "bitscore")]
      } else {
        data.frame(blast=c("output1", "output2"), query=c("input1", "input2"), no_blast = c("no blast results", "not here..."), stringsAsFactors = F)
      }
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        blast_out, server = FALSE, escape = FALSE, selection = 'none'
      )
      }
      
    }
  })
  
  # The dynamic input definition
  output$dynamicInput_ITS <- renderUI({
  if (input$is_extracted == FALSE) {
    fluidRow(
      column(4,selectInput(ns("data_its"), "EXTRACT ITS:", choices = c("ITS1", "ITS2")))
    )
  } else {
    return(NULL)
  }
  })
  
  # redirect...  
  observeEvent(input$lastClick, {
    selectedRow <- as.numeric(strsplit(input$lastClickId, "_")[[1]][2])
    md5_hash <- toString(vals$seq_hash[selectedRow,"sseqid"])
    print(md5_hash)
    #info about result type...
    vals$type =  "sequence"
    vals$text <- paste("BLAST SIMILARITY:",vals$seq_hash[selectedRow,"pident"],input$textSeq)
    #pass code of the study...
    vals$key <- md5_hash
    callModule(session = parent, module = resultsFunc, id = "id_results", vals)
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  }
  )
}
