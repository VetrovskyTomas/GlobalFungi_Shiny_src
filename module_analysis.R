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
          "CAACCCTCAAGCTCTGCTTGGTATTGGGCTACACCCGACTGGGTGGGCCTTAAAATCAGTGGCGGTGCCATCTGGCTCTAAGCGTAGTAATTCTTCTCGCTCTGGAGATCTAGGTGTTTGCTTGTCAGCAACCCCCAATTTATCAAA", 
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
          column(8,selectInput(ns("search_type"), "Type:", choices = c("Exact", "BLAST")))
        ),
        hr(),
        fluidRow(
          column(6,actionButton(ns("analyze_button"), label = "Analyze"))
        ),
        hr(),
        fluidRow(
          column(12,tableOutput(ns('info_table')))
        )
      )
    
  )
}

# Function for module server logic
analysisFunc <- function(input, output, session, parent) {
  # to be shared...
  vals <- reactiveValues()
  
  # When user clicks on submit button : Update result tab...
  observeEvent(input$analyze_button, {
    if (input$search_type == "Exact"){
      print("Searching for exact sequence match...")
      vals$type =  as.character("sequence")
      vals$text <- input$textSeq
      callModule(session = parent, module = resultsFunc, id = "id_results",vals, parent = parent)    
      updateTabItems(session = parent, "menu_tabs", "fmd_results")
    } else {
      print("Run the blast...")

      # generate folder for user task...
      outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
      system(paste("mkdir ", outputDir, sep = "")) #for linux

      # write input fasta...
      input_fasta <- data.frame(seq1 = c(">query_seq", input$textSeq), stringsAsFactors = F)
      write.table(x = input_fasta,
                  file = paste(outputDir,"my_query.fasta", sep = ""),
                  quote = F, col.names = F, row.names = F)
      
      # run blast command...
      cmd_blast <- paste0("blastn -db /home/fungal/databases/blast_database/my_self_made_db.fasta -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out -outfmt 6")
      system(cmd_blast)
      
      # Check if your blast finished ## please lets improve this to something more cleaver ##
      if(!dir.exists(outputDir)){
        output$info_table <- renderTable({
          data.frame(blast=c("output1", "output2"), query=c("input1", "input2"), no_blast = c(paste0("Error in creation of folder: ", outputDir), "not here..."), stringsAsFactors = F)
        })
      } else {
        output$info_table <- renderTable({
          blast_out <- read.delim(file = paste0(outputDir, "results.out"), header = F)
          blast_out
        })
      }
    }
  })
  
  #namespace for dynamic input...
  ns <- session$ns
  
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
  
}
