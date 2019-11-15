# Function for module UI
analysisGroupUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    h1(id="welcome_title", "Analyze sequence!"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste you sequence", 
          "AATCCTCAAAACCTGTTCTTCGATTGGGGAAAGGGTTTTGGACTTGGAGGATCAATGCTTGCCCTCACTTTGAAGGCGAGCTCCTCTCAAATAAATTAGTGGGGCTTGCTTTGCTGATCCTTGACGGTGATAAGACATCTCTACGTTTTGGATTTGGCGACTGTCCCTTGCTTCTAACCGTCCTATGGACAATGATGGGTGCACCGGTCTACCACCTTACATTGGTGGGGGGCTGGACCCACAAAAAGGA", 
          width="100%",
          height = "200px"),
        fluidRow(
          column(3,br(),textInput(ns("max_blast_results"), label = "Max of BLAST results:", "500")),
          column(3,br(),actionButton(ns("analyze_button"), label = "Analyze", icon = icon("dna")))
        ),
        hr(),
        # info about subbmission...
        verbatimTextOutput(ns('info_fasta')),
        uiOutput(ns('dynamic_filters')),
        hr(),
        # table of results... 
        fluidRow(
          column(12,DT::dataTableOutput(ns('info_table')))
        )
      )
    
  )
}

# Function for module server logic
analysisGroupFunc <- function(input, output, session, parent) {
  
  #namespace for dynamic input...
  ns <- session$ns
  
  # to be shared...
  vals <- reactiveValues()
  filtered_data <- reactiveValues()
  #filtered_data <- NULL
  #vals <- NULL
  
  
  ##################################################################
  # process fasta
  fasta_out <- function(fasta_vector, as_table) {
    names <- vector()
    seqs <- vector()
    seq <- ""
    for (row in 1:length(fasta_vector)) {
      line <- toString(fasta_vector[row])
      first_char <- substring(line, 1, 1)
      if (first_char == ">"){
        if (seq != ""){
          seqs <- c(seqs, seq)
        }
        if (as_table == TRUE){
          names <- c(names,substring(line, 2))
        } else {
          names <- c(names,line)
        }
        seq <- ""
      } else {
        seq <- paste0(seq,line)
      }
    }
    seqs <- c(seqs, seq)
    #
    data.frame(titles = as.character(names), sequences = as.character(seqs), stringsAsFactors = F)
  }
  
  # test FASTA parameters...
  get_fasta <- function(fasta_data) {
    # test counts...
    if (nrow(fasta_data) > 100){
      alert(paste0("Too many sequences ",nrow(fasta_data),". Maximum number of sequences is 100!"))
      return(NULL)
    }
    # test length...
    if (max(nchar(fasta_data$sequences)) > 4000){
      alert(paste0("Too long sequence - ",max(nchar(fasta_data$sequences))," bp. Maximum length of sequences is 4000 bp!"))
      return(NULL)
    }
    # get info about FASTA...
    output$info_fasta <- renderText({
      seq_num <- nrow(fasta_data)
      max_len <- max(nchar(fasta_data$sequences))
      return(paste0("You have submitted: \n", seq_num, " sequence(s) \n", "with the maximal length ",max_len," bp \n"))
    })
    # return input fasta...
    return(fasta_data)
  }
  
  # add buttons...
  shinyInput <- function(FUN, len, id, label, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label[i], ...))
    }
    inputs
  }
  
  # simulate BLAST
  blast <- function(input_fasta, vals) {
    withProgress(message = 'Running BLAST...', {
      # generate folder for user task...
      outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
      system(paste("mkdir ", outputDir, sep = ""))

      write.table(x = do.call(rbind, lapply(seq(nrow(input_fasta)), function(i) t(input_fasta[i, ]))), file = paste(outputDir,"my_query.fasta", sep = ""), quote = F, col.names = F, row.names = F)
      incProgress(1/5)

      # run blast command...
      print(paste("Maximum of blast results:",input$max_blast_results))
      cmd_params <- paste0("-outfmt 6 -max_target_seqs ",input$max_blast_results," -num_threads 2")
      cmd_blast <- paste0("blastn -db ", global_blast_db," -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out ", cmd_params)
      system(cmd_blast)
      incProgress(1/5)

      # Check if your blast finished...
      blast_out <- NULL
      if(file.exists(paste0(outputDir, "results.out"))){
        blast_out <- read.delim(file = paste0(outputDir, "results.out"), header = F)
      }
      incProgress(1/5)

      # remove folder after use...
      system(paste0("rm -rf ",outputDir))
      
      ###################################
      # Check if your blast finished...
      # blast_out <- NULL
      # if(file.exists(paste0("results.out"))){
      #   blast_out <- read.delim(file = paste0("results.out"), header = F)
      # }
      ###################################
      
      # prepare output...
      if (!is.null(blast_out)){
        names(blast_out) <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
        blast_out <- blast_out[,c("qseqid", "sseqid", "pident", "qstart", "qend", "sstart", "send", "evalue", "bitscore")]
      } else {
        blast_out <- data.frame(qseqid = vector(), sseqid = vector(), hits = vector(), pident = vector(), qstart = vector(), qend = vector(), sstart = vector(), send = vector(), evalue = vector(), bitscore = vector(), stringsAsFactors = F)
      }
      incProgress(1/5)
      
      vals$blast_out <- blast_out
      if (nrow(blast_out)>0){
        filtered_data$blast_out <- blast_out
      } else {
        filtered_data$blast_out <- NULL
      }
      #print(filtered_data$blast_out)
      
      # get info results...
      output$info_fasta <- renderText({
        return(paste0("BLASTn resulted in ",nrow(vals$blast_out)," sequences."))
      })
      
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        DT::datatable({
          vals$blast_out
        }, escape = FALSE, selection = 'none')
      )
      incProgress(1/5)
    })
    return(vals)
  }  
  #################################################

  
  
  # analyze...
  observeEvent(input$analyze_button, {
      input_fasta <- NULL

      if (input$textSeq != '') {
          # reading from text-area...
          print("reading from text-area...")
          # simple nucleotide text...
          seq <- gsub("[\r\n]", "", input$textSeq)
          vals$seq <- seq
          input_fasta <- get_fasta(data.frame(titles = ">query", sequences = as.character(seq), stringsAsFactors = F))
        } else {
          alert(paste0("Input sequence is empty!"))
        }
      
      
      # 
      if (!is.null(input_fasta)) {
        print("FASTA IS OK...")
        vals <- blast(input_fasta, vals)
        #
      } else {
        print("ERROR: CRITERIA NOT FULFILLED...")
      }
    #}
  })
  
  
  # dynamic filters...
  output$dynamic_filters <- renderUI({
    if (!is.null(vals$blast_out)){
      # variables...
      similarity <- as.numeric(vals$blast_out$pident)
      # filters...
      wellPanel(
      fluidRow(
        column(10,sliderInput(ns("similarity"), "Similarity:",
                              min = min(similarity), max = max(similarity), value = c(min(similarity),max(similarity)), step = 0.001, sep = "")
        )
        ,column(2,actionButton(ns("applyFilters"), "Apply filter", icon = icon("filter")))
      ),
      hr(),
      fluidRow(
        column(2,actionButton(ns("getSamples"), "Show samples", icon = icon("dna")))
      )
      )
    }
  })
  
  # apply filter...
  observeEvent(input$applyFilters, {
    if (!is.null(vals$blast_out)){
      filtered_data$blast_out <- isolate(vals$blast_out)
      # similarity
      sim <- isolate(input$similarity)
      filtered_data$blast_out <- filtered_data$blast_out[which(as.numeric(filtered_data$blast_out$pident) >= sim[1]),]
      filtered_data$blast_out <- filtered_data$blast_out[which(as.numeric(filtered_data$blast_out$pident) <= sim[2]),]
      # show updated table...
      output$info_table <- DT::renderDataTable(
        DT::datatable({
          filtered_data$blast_out
        }, escape = FALSE, selection = 'none')
      )
      
      # get info results...
      output$info_fasta <- renderText({
        return(paste0("BLASTn resulted in ",nrow(vals$blast_out)," sequences.\n After filtering ",nrow(filtered_data$blast_out)," sequences."))
      })
    }
  })
  
  # apply filter...
  observeEvent(input$getSamples, {
    print("get samples...")
    print(filtered_data$blast_out)
    #info about result type...
    vals$type =  "sequence"
    vals$key <- filtered_data$blast_out$sseqid
    vals$text <- paste0("BLAST group result (", length(vals$key)," sequence variants) for query:\n", vals$seq) #paste0(vals$seq_hash[selectedRow,"qseqid"]," BEST SIMILARITY: ",vals$seq_hash[selectedRow,"pident"],"\n",vals$seq_hash[selectedRow,"sequence"])

    
    # call results...
    callModule(session = parent, module = resultsFunc, id = "id_results",isolate(vals)) 
    updateTabItems(session = parent, "menu_tabs", "fmd_results")    
  })
  
}
