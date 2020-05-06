# Function for module UI
analysisUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      #div(style = "padding: 0px 0px;margin-top:-1em;margin-bottom:0em;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='search_seq.png', height = 56)),
        column(11, h2(id="header_title", "Search by sequence!"))
      )
      #)
    ),
    # Sidebar panel for inputs ----
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste your sequence", 
          "CCGAAGTACAGGCCCTCTCGTAGGGCTAAACTTCCACCCTTTGTTTATCATACCATGTTGCTTTGGCGAGACGTCCTCGGACCACCGGCCCTCGGGCGGGTGCGCGCTCGCCAGAGAAAAATCAAACCCAAACCATTTTAGTAGTAGTCTGAAAACAAGTTTCAATTATTA", 
          width="100%",
          height = "200px"),
        
        # Input: Select a file ----
        fluidRow(
          column(9,fileInput(ns("fasta_file"), "Choose FASTA file", multiple = FALSE, accept = c("text/plain",".fasta",".fa",".fas"))),
          column(3,br(),actionButton(ns("reset_file"), 'Reset/Clear Input'))
        ),
        fluidRow(
          column(5,radioButtons(ns("search_type"), "Search type:", c("Exact hit (input 1-100 sequences; only complete ITS1 or ITS2)" = "exact","BLAST - best hit (input 1-100 sequences; ITS1 or ITS2)" = "blast", "BLAST - group results (input 1 sequence; ITS1 or ITS2)" = "blast_group"))),
          column(5,uiOutput(ns('dynamic_params'))),
          column(2,actionButton(ns("analyze_button"), label = "Search", icon = icon("dna")))
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
analysisFunc <- function(input, output, session, parent) {
  
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
  
  # show results...
  show_results <- function(vals) {
    callModule(session = parent, module = resultsFunc, id = "id_results",isolate(vals))
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  }
  ##################################################################
  # dynamic header - seq vars...
  output$dynamic_params <- renderUI({
    if (input$search_type == "blast_group"){
      fluidRow(
        column(3,
              selectizeInput(ns("max_blast_results"),
                       options = list(
                         maxOptions=length(global_species_list),
                         placeholder = 'Please select an option below'
                       ),
                       label = "Max of BLAST results:", choices = c(10, 50, 100, 500), width = "300px",
                       selected = 1,
                       multiple = FALSE # allow for multiple inputs
                       )
                       )
      )
    }
  })
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
  
  # run EXACT
  exact <- function(input_fasta, vals) {
    print("exact...........")
    withProgress(message = 'Running exact search...', {
      # modify fasta dataframe...
      
      input_fasta$titles <- substring(input_fasta$titles, 2)
      input_fasta$md5 <- as.character(sapply(input_fasta$sequences, digest, algo="md5", serialize=F))
      
      # Construct the fetching query
      key_string <- paste0("('",paste(input_fasta$md5, collapse="','" ),"')")
      query <- sprintf(paste0("SELECT * from ",options()$mysql$variants_table," WHERE `hash` IN ",key_string))
      variants <- sqlQuery(query)
      
      cat(file=stderr(), "input_fasta size is ", nrow(variants), "\n")
      #print(paste0("Found variants...", nrow(variants)))
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
      )
      incProgress(1/2)
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
      cmd_params <- paste0("-use_index true -outfmt 6 -max_target_seqs 1 -num_threads ", global_blast_nproc)
      cmd_blast <- paste0("blastn -db ", global_blast_db," -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out ", cmd_params)
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
  
  # simulate BLAST
  blast_group <- function(input_fasta, vals) {
    print("blast group...........")
    withProgress(message = 'Running BLAST...', {
      # generate folder for user task...
      outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
      system(paste("mkdir ", outputDir, sep = ""))
      
      write.table(x = do.call(rbind, lapply(seq(nrow(input_fasta)), function(i) t(input_fasta[i, ]))), file = paste(outputDir,"my_query.fasta", sep = ""), quote = F, col.names = F, row.names = F)
      incProgress(1/5)
      
      # run blast command...
      maxres <- input$max_blast_results
      print(paste("Maximum of blast results:",maxres))
      cmd_params <- paste0("-use_index true -outfmt 6 -max_target_seqs ",maxres," -num_threads ", global_blast_nproc)
      cmd_blast <- paste0("blastn -db ", global_blast_db," -query ",outputDir, "my_query.fasta -out ", outputDir,"results.out ", cmd_params)
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
  observeEvent(input$analyze_button, {
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
        if (search$search_type == "exact"){
          vals$type <- "sequence"
          vals <- isolate(exact(input_fasta, vals))
        }
        # blast simple...
        if (search$search_type == "blast"){
          vals$type <- "single-blast"
          vals <- isolate(blast(input_fasta, vals))
        }
        # blast group...
        if (search$search_type == "blast_group"){
          if (nrow(input_fasta)==1){
          vals$type <- "multi-blast"
          vals$seq <- input_fasta[[2]]
          vals <- isolate(blast_group(input_fasta, vals))
          } else {
            alert(paste0("Sorry, BLAST for group is allowed only  for one FASTA sequence (you have ", nrow(input_fasta),")"))
          }
        }
      } else {
        print("ERROR: CRITERIA NOT FULFILLED...")
      }
  })
  
  # dynamic filters...
  output$dynamic_filters <- renderUI({
    if (!is.null(filtered_data$blast_out)&&(input$search_type == "blast_group")){
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
      print("apply filter...")
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
      data$text <- paste0(vals$seq_hash[selectedRow,"qseqid"]," BEST SIMILARITY: ",vals$seq_hash[selectedRow,"pident"],"\n",vals$seq_hash[selectedRow,"sequence"])
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
