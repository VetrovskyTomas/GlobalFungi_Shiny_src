# Function for module UI
clustersUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      #div(style = "padding: 0px 0px;margin-top:-1em;margin-bottom:0em;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='settings.png', height = 56)),
        column(11, h2(id="header_title", "Cluster analysis!"))
      )
      #)
    ),
    # Sidebar panel for inputs ----
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste your sequence", 
          paste(""),
          #"",
          placeholder = "CCGAAGTACAGGCCCTCTCGTAGGGCTAAACTTCCACCCTTTGTTTATCATACCATGTTGCTTTGGCGAGACGTCCTCGGACCACCGGCCCTCGGGCGGGTGCGCGCTCGCCAGAGAAAAATCAAACCCAAACCATTTTAGTAGTAGTCTGAAAACAAGTTTCAATTATTA",
          width="100%",
          height = "120px"),
        
        # Input: Select a file ----
        fluidRow(
          column(9,fileInput(ns("fasta_file"), "Choose FASTA file", multiple = FALSE, accept = c("text/plain",".fasta",".fa",".fas"))),
          column(3,br(),actionButton(ns("reset_file"), 'Reset/Clear Input'))
        ),
        fluidRow(
          column(12,
                 #radioButtons(ns("database_type"), "Marker type:", c("ITS2" = "ITS2"))),
          
                 radioButtons(ns("database_type"), 
                             "Marker type:",
                             choiceNames = global_clusters_db_names, 
                             choiceValues = global_clusters_db_paths,
                             selected = "/home/fungal/databases/blast_database/CLUSTERS_ITS1.fas"
          )),
          column(2,actionButton(ns("blast_button"), label = "BLAST - best hit (input 1-100 sequences)", icon = icon("dna")))
        ),
        hr(),
        # info about subbmission...
        verbatimTextOutput(ns('info_fasta')),
        uiOutput(ns('dynamic_filters')),
        # table of results... 
        fluidRow(
          column(12,DT::dataTableOutput(ns('info_table')))
        )
      )
    
  )
}

# Function for module server logic
clustersFunc <- function(input, output, session, parent) {
  #namespace for dynamic input...
  ns <- session$ns
  
  # to be shared...
  vals <- reactiveValues()
  filtered_data <- reactiveValues()
  vals$seq_hash <- NULL
  vals$type <- NULL
  vals$text <- NULL
  vals$key <- NULL
  
  search <- reactiveValues(search_type = NULL)
  
  selected.rows <- reactiveValues(index = NULL)
  
  observeEvent(input$lastClick, {
    selected.rows$index <- as.numeric(strsplit(isolate(input$lastClickId), "_")[[1]][2])
  })
  
  blast_db <- reactive({
    paste0("'",paste(input$database_type,collapse=" "),"'")
  })
  
  observeEvent(input$blast_db, {
    print(paste("BLAST DB:", blast_db()))
  })
  
  # show results...
  show_results <- function(vals) {
    callModule(session = parent, module = resultsFunc, id = "id_results",isolate(vals))
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  }
  ##################################################################
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
    # test length...
    if (max(nchar(fasta_data$sequences)) < 40){
      alert(paste0("Too short sequence - ",max(nchar(fasta_data$sequences))," bp. Minimum length of sequences is 40 bp!"))
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
  
  #################################################
  # run BLAST TEST
  blast_test <- function(input_fasta, vals) {
    print("blast...........")
    withProgress(message = 'Running BLAST...', {
      blast_out <- read.delim(file = paste0(global_out_path, "results.out"), header = F)
      
      print(blast_out)
      
      # prepare output...
      if (!is.null(blast_out)){
        names(blast_out) <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
        blast_out$hits <- shinyInput(actionButton, nrow(blast_out), 'button_', label = blast_out[,"sseqid"],
                                     onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                      "Shiny.onInputChange('", ns("lastClick"), "', Math.random())"))
        blast_out <- blast_out[,c("qseqid", "sseqid", "hits", "pident", "qstart", "qend", "sstart", "send", "evalue", "bitscore")]
      } else {
        blast_out <- data.frame(qseqid = vector(), sseqid = vector(), hits = vector(), pident = vector(), qstart = vector(), qend = vector(), sstart = vector(), send = vector(), evalue = vector(), bitscore = vector(), stringsAsFactors = F)
      }
      incProgress(1/5)
      
      # finish the table
      input_fasta$titles <- substring(input_fasta$titles, 2)
      input_fasta$hits <- blast_out$hits[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$md5 <- blast_out$sseqid[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$similarity <- blast_out$pident[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$qstart <- blast_out$qstart[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$qend <- blast_out$qend[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$sstart <- blast_out$sstart[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$send <- blast_out$send[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$evalue <- blast_out$evalue[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$bitscore <- blast_out$bitscore[match(input_fasta$titles, blast_out$qseqid)]
      
      # create seq_hash data.frame
      vals$seq_hash <- data.frame(sseqid = input_fasta$md5, qseqid = input_fasta$titles,pident = input_fasta$similarity, sequence = input_fasta$sequences, stringsAsFactors = F)
      
      # select the columns...
      input_fasta <- input_fasta[,c("titles","hits","similarity","qstart","qend","sstart","send","evalue","bitscore")]
      
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        DT::datatable({
          input_fasta <- input_fasta %>% mutate(hits = ifelse(!is.na(hits), hits, "NO HIT"))
          input_fasta
        }, escape = FALSE, selection = 'none')
      )
      incProgress(1/5)
    })
    return(vals)
  }
  # run BLAST
  blast <- function(input_fasta, vals) {
    print("blast...........")
    withProgress(message = 'Running BLAST...', {
      # generate folder for user task...
      outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
      system(paste("mkdir ", outputDir, sep = ""))
      
      write.table(x = do.call(rbind, lapply(seq(nrow(input_fasta)), function(i) t(input_fasta[i, ]))), file = paste(outputDir,"my_query.fasta", sep = ""), quote = F, col.names = F, row.names = F)
      incProgress(1/5)
      
      # run blast command...
      cmd_params <- paste0("-use_index true -outfmt 6 -max_target_seqs 10 -num_threads ", global_blast_nproc)
      cmd_blast <- paste0("blastn -task megablast -db ", blast_db()," -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out ", cmd_params)
      print(cmd_blast)
      system(cmd_blast)
      incProgress(1/5)
      
      # Check if your blast finished...
      blast_out <- NULL
      filename <- paste0(outputDir, "results.out")
      if(file.exists(filename)){
        info = file.info(filename)
        if (info$size == 0){
          print(paste0("File ", filename, " is empty!"))
        } else {
          blast_out <- read.delim(file = paste0(outputDir, "results.out"), header = F)
        }
      }
      incProgress(1/5)
      
      # remove folder after use...
      system(paste0("rm -rf ",outputDir))
      
      # prepare output...
      if (!is.null(blast_out)){
        names(blast_out) <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
        blast_out$hits <- shinyInput(actionButton, nrow(blast_out), 'button_', label = blast_out[,"sseqid"],
                                     onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                      "Shiny.onInputChange('", ns("lastClick"), "', Math.random())"))
        blast_out <- blast_out[,c("qseqid", "sseqid", "hits", "pident", "qstart", "qend", "sstart", "send", "evalue", "bitscore")]
      } else {
        blast_out <- data.frame(qseqid = vector(), sseqid = vector(), hits = vector(), pident = vector(), qstart = vector(), qend = vector(), sstart = vector(), send = vector(), evalue = vector(), bitscore = vector(), stringsAsFactors = F)
      }
      incProgress(1/5)
      
      # finish the table
      input_fasta$titles <- substring(input_fasta$titles, 2)
      input_fasta$hits <- blast_out$hits[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$md5 <- blast_out$sseqid[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$similarity <- blast_out$pident[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$qstart <- blast_out$qstart[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$qend <- blast_out$qend[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$sstart <- blast_out$sstart[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$send <- blast_out$send[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$evalue <- blast_out$evalue[match(input_fasta$titles, blast_out$qseqid)]
      input_fasta$bitscore <- blast_out$bitscore[match(input_fasta$titles, blast_out$qseqid)]
      
      # create seq_hash data.frame
      vals$seq_hash <- data.frame(sseqid = input_fasta$md5, qseqid = input_fasta$titles,pident = input_fasta$similarity, sequence = input_fasta$sequences, stringsAsFactors = F)
      
      # select the columns...
      input_fasta <- input_fasta[,c("titles","hits","similarity","qstart","qend","sstart","send","evalue","bitscore")]
      
      # reder blast results table...
      output$info_table <- DT::renderDataTable(
        DT::datatable({
          input_fasta <- input_fasta %>% mutate(hits = ifelse(!is.na(hits), hits, "NO HIT"))
          input_fasta
        }, escape = FALSE, selection = 'none')
      )
      incProgress(1/5)
    })
    return(vals)
  }
  #################################################

  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$search_type, {
    search$search_type <- isolate(input$search_type)
  })
  
  observeEvent(input$fasta_file, {
    print("File uploaded...")
    values$upload_state <- 'uploaded'
    disable("textSeq")
  })
  
  observeEvent(input$reset_file, {
    print("Clicked reset...")
    updateTextInput(session,"textSeq", value="")
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

  # analyze...
  observeEvent(input$blast_button, {
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
          alert(paste0("Input FASTA sequence is empty!!!"))
        }
      }
      # 
      if (!is.null(input_fasta)) {
        print(paste0("FASTA IS OK...type ",input$search_type," #of seqs. ", nrow(input_fasta) ," max res: ", input$max_blast_results))
        
        # exact match...
        vals$type <- "cluster-blast"
        #vals <- isolate(blast_test(input_fasta, vals))
        vals <- isolate(blast(input_fasta, vals))
      } else {
        print("ERROR: CRITERIA NOT FULFILLED...")
      }
  })
  
  # redirect...  
  observeEvent(input$lastClick, {
    # to be shared...
    data <- reactiveValues()
    data$type <- isolate(vals$type)
    data$text <- 'No results yet!'
    data$key <- ''
    #
    if (!is.null(selected.rows$index)){
      print("redirect...")
      selectedRow <- selected.rows$index
      
      print(paste("selectedRow",selectedRow))
      # get md5 hash...
      md5_hash <- toString(vals$seq_hash[selectedRow,"sseqid"])
      print(md5_hash)
      
      #info about result type...
      data$text <- md5_hash #paste0(md5_hash," (SIMILARITY: ",vals$seq_hash[selectedRow,"pident"]," to ",vals$seq_hash[selectedRow,"qseqid"],")")
      #pass code of the study...
      data$key <- md5_hash
      # call results...
      show_results(data)
    }
  }, ignoreInit = TRUE)
  
  # show samples...
  observeEvent(input$getSamples, {
    # to be shared...
    data <- reactiveValues()
    data$type <- isolate(vals$type)
    data$text <- 'No results yet!'
    data$key <- ''
    #
    print("get samples...")
    #info about result type...
    data$key <- unique(filtered_data$blast_out$sseqid)
    data$text <- paste0("BLAST group result (", length(data$key)," sequence variants) for query:\n", vals$seq)
    # call results...
    show_results(data)
  })
  
}
