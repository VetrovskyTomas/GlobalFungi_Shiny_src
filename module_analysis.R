# Function for module UI
analysisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    h1(id="welcome_title", "Analyze sequences!"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste you sequence", 
          "ATGAATCAGCACCGTGTTTTTGTTAATGCGGAGGCTGGACTTGGATGTTGCCGTCGTTGGCTCGTCCTTAAATGCATTAGCCTGGCGCGGCCAAGGCCTCTTTGGTGTGATAATTATCTACGCTGCTGGGGTTTGTGCTTTGTTGCTGGCTTACAGTCGTCCTCGGACAACTTTTTTTG", 
          width="100%",
          height = "200px"),
        
        # Input: Select a file ----
        fluidRow(
          column(9,fileInput(ns("fasta_file"), "Choose FASTA file", multiple = FALSE, accept = c("text/plain",".fasta",".fa",".fas"))),
          column(3,br(),actionButton(ns("reset_file"), 'Reset Input'))
        ),
        fluidRow(
          column(3,selectInput(ns("search_type"), "Type:", choices = c("BLAST", "Exact"))),
          column(6,br(),actionButton(ns("analyze_button"), label = "Analyze", icon = icon("dna")))
        ),
        hr(),
        # info about subbmission...
        verbatimTextOutput(ns('info_fasta')),
        # table of results... 
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
  
  # run EXACT
  exact <- function(input_fasta, vals) {
    withProgress(message = 'Running exact search...', {
      # modify fasta dataframe...
      
      input_fasta$titles <- substring(input_fasta$titles, 2)
      input_fasta$md5 <- as.character(sapply(input_fasta$sequences, digest, algo="md5", , serialize=F))
      
      
      variants <- global_variants[which(global_variants$hash %in% input_fasta$md5),]
      print(paste0("Found variants...", nrow(variants)))
      input_fasta$found <- input_fasta$md5[match(input_fasta$md5, variants$hash)]
      
      input_fasta$hits <- shinyInput(actionButton, nrow(input_fasta), 'button_', label = input_fasta[,"found"],
                                     onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                      "Shiny.onInputChange('", ns("lastClick"), "', Math.random())"))
      # create seq_hash data.frame
      vals$seq_hash <- data.frame(sseqid = input_fasta$md5,qseqid = input_fasta$titles,pident = rep("100", nrow(input_fasta)),sequence = input_fasta$sequences, stringsAsFactors = F)
      
      # done...
      input_fasta <- input_fasta[,c("titles", "md5" ,"hits")]

     incProgress(1/2)
      
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        DT::datatable({
          input_fasta <- input_fasta %>% mutate(hits = ifelse(!grepl("NA", hits), hits, "NOT FOUND"))
          input_fasta
        }, escape = FALSE, selection = 'none')
        #input_fasta, server = FALSE, escape = FALSE, selection = 'none'
      )
      incProgress(1/2)
    })
    return(vals)
  }  
  
  # run BLAST
  blast <- function(input_fasta, vals) {
    withProgress(message = 'Running BLAST...', {
      # generate folder for user task...
      outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
      system(paste("mkdir ", outputDir, sep = ""))
      
      write.table(x = do.call(rbind, lapply(seq(nrow(input_fasta)), function(i) t(input_fasta[i, ]))), file = paste(outputDir,"my_query.fasta", sep = ""), quote = F, col.names = F, row.names = F)
      incProgress(1/5)
      
      # modify fasta dataframe...
      input_fasta$titles <- substring(input_fasta$titles, 2)
      
      # run blast command...
      cmd_params <- "-outfmt 6 -max_target_seqs 1 -num_threads 2"
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
      
      # prepare output...
      if (!is.null(blast_out)){
        names(blast_out) <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
        vals$seq_hash <- blast_out[,c("qseqid", "sseqid", "pident")]
        # add sequences...
        vals$seq_hash$sequence <- input_fasta$sequences[match(vals$seq_hash$qseqid, input_fasta$titles)]
        #
        blast_out$hits <- shinyInput(actionButton, nrow(blast_out), 'button_', label = blast_out[,"sseqid"],
                                     onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                      "Shiny.onInputChange('", ns("lastClick"), "', Math.random())"))
        blast_out <- blast_out[,c("qseqid", "hits", "pident", "qstart", "qend", "sstart", "send", "evalue", "bitscore")]
      } else {
        blast_out <- data.frame(status=c("Output not found!"), stringsAsFactors = F)
      }
      incProgress(1/5)
      
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        blast_out, server = FALSE, escape = FALSE, selection = 'none'
      )
      incProgress(1/5)
    })
    return(vals)
  }  
  #################################################

  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$fasta_file, {
    print("File uploaded...")
    values$upload_state <- 'uploaded'
    disable("textSeq")
  })
  
  observeEvent(input$reset_file, {
    print("Clicked reset...")
    values$upload_state <- 'reset'
    enable("textSeq")
    reset("fasta_file")
  })
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$fasta_file)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })

  # test
  # observeEvent(input$test_button, {
  #     # Searching for exact sequence match...
  #     vals$type =  "sequence"
  #     vals$text <- gsub("[\r\n]", "", input$textSeq)
  #     vals$key <- md5_hash <- as.character(digest(vals$text, algo="md5", serialize=F))
  #     print(vals$key)
  #     # call funtion...
  #     callModule(session = parent, module = resultsFunc, id = "id_results",vals)
  #     updateTabItems(session = parent, "menu_tabs", "fmd_results")
  # })
  
  # analyze...
  observeEvent(input$analyze_button, {
    # if (input$search_type == "Exact"){
    #   # Searching for exact sequence match...
    #   vals$type =  "sequence"
    #   vals$text <- gsub("[\r\n]", "", input$textSeq)
    #   vals$key <- md5_hash <- as.character(digest(vals$text, algo="md5", serialize=F))
    #   # call funtion...
    #   callModule(session = parent, module = resultsFunc, id = "id_results",vals)
    #   updateTabItems(session = parent, "menu_tabs", "fmd_results")
    # } else {

      input_fasta <- NULL

      # load fasta from file...
      if (!is.null(file_input())) {
        # reading from file...
        print("reading from file...")
        fasta_file_text <- scan(file = file_input()$datapath, character(), quote = "")
        input_fasta <- get_fasta(fasta_out(fasta_file_text, FALSE))
      } else {
        if (input$textSeq != '') {
          # reading from text-area...
          print("reading from text-area...")
          if (grepl(">", input$textSeq)){
            # fasta formate (sigle/multiple)...
            input_fasta <- get_fasta(fasta_out(stri_split_lines(input$textSeq)[[1]], FALSE))
          } else {
            # simple nucleotide text...
            seq <- gsub("[\r\n]", "", input$textSeq)
            input_fasta <- get_fasta(data.frame(titles = ">query", sequences = as.character(seq), stringsAsFactors = F))
          }
        } else {
          print("empty...")
        }
      }
      
      # 
      if (!is.null(input_fasta)) {
        print("FASTA IS OK...")
        #print(input_fasta)
        #cat(file=stderr(), "input_fasta size is ", nrow(input_fasta), "\n")
        if (input$search_type == "Exact"){
          vals <- exact(input_fasta, vals)
        } else {
          vals <- blast(input_fasta, vals)
        }
        #
      } else {
        print("ERROR: CRITERIA NOT FULFILLED...")
      }
    #}
  })
  
  # redirect...  
  observeEvent(input$lastClick, {
    selectedRow <- as.numeric(strsplit(input$lastClickId, "_")[[1]][2])
  
    print(paste("selectedRow",selectedRow))
    # get md5 hash...
    md5_hash <- toString(vals$seq_hash[selectedRow,"sseqid"])
    print(md5_hash)
    
    #info about result type...
    vals$type =  "sequence"
    vals$text <- paste0(vals$seq_hash[selectedRow,"qseqid"]," BEST SIMILARITY: ",vals$seq_hash[selectedRow,"pident"],"\n",vals$seq_hash[selectedRow,"sequence"])
    #pass code of the study...
    vals$key <- md5_hash
    callModule(session = parent, module = resultsFunc, id = "id_results", vals)
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  }
  )
}
