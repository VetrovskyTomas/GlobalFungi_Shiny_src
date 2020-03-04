# Function for module UI with SH
resutsVariantsUI <- function(id) {
  ns <- NS(id)
  
  # show variants ui
    column(2,
           textOutput(ns("seq_vars_count")),
           downloadButton(ns("downloadSeqVars"), "Download FASTA"),
           checkboxInput(ns("seqs_derep"), "dereplicated", TRUE),
           bsTooltip(ns("seqs_derep"), "This checkbox allows you to have sample names in the sequence titles.",
                     "right", options = list(container = "body"))
    )
}

# Function for module server logic
resutsVariantsFunc <- function(input, output, session,  data, type, text) {
  #namespace for dynamic input...
  ns <- session$ns  

  # use taxon name for fast search
  taxon <- text
  
  type <- gsub(" ", "_", type)
  text <- gsub(" ", "_", text)
  
  if (text == ""){
    text <- "variant"
  }
  
  if (type == ""){
    type <- "sequence"
  }
  
  print(paste0("Resuts Variants Func - ",type, " ", text, " - samples ",("samples" %in% colnames(data$SeqVars))))
  
  # Downloadable fasta of selected sequence variants...
  output$downloadSeqVars <- downloadHandler(
    filename = "sequences.zip",
    content = function(file) {
      if (!is.null(data$SeqVars)){
        seqVars <- NULL
        ##################################################
        if (!"samples" %in% colnames(data$SeqVars)){
          withProgress(message = 'Getting sequence info:', {
            incProgress(1/3, detail = "This may take a while...")
            # search by taxon...
            query <- paste0("SELECT * from ",options()$mysql$taxonomy," WHERE `",type,"` = '",taxon,"'")
            taxons <- sqlQuery(query)
            taxid <- unique(taxons[,paste0(type,"_id")])
            print(taxid)
            
            # Construct the fetching query
            query <- paste0("SELECT `samples`,`marker`,`sequence` from ",options()$mysql$variants_table," WHERE `",type,"` = '",taxid,"'")
            seqVars <- data.frame(sqlQuery(query))
            incProgress(1/3, detail = "Processing output...")
            print(paste0("Number of seqs found is ", nrow(seqVars)))
            #seqVars <- variants[,c("samples", "hash", "marker")]
          })
        } else {
          seqVars <- data$SeqVars
        }
        ##################################################
        withProgress(message = 'Extracting sequences:', {
          incProgress(1/5, detail = "Creation of folder...")
          # generate folder for user task...
          outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
          system(paste("mkdir ", outputDir, sep = "")) #for linux
          incProgress(1/5, detail = "Writting info...")
          # write fasta titles...
          #input_titles <- data.frame(titles = seqVars[,"hash"], stringsAsFactors = F)
          write.table(seqVars,file = paste0(outputDir,"seq_info.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE, sep="\t")
          
          # python
          incProgress(1/5, detail = "Generating FASTA...")
          system(paste0("python ", global_vars_to_fasta_py," ", outputDir,"seq_info.txt ",input$seqs_derep," ",type," ",text," ",outputDir,"out.fa"))

          incProgress(1/5, detail = "Commpressing FASTA...")
          system(paste0("zip -j ", paste0(outputDir, "out.fa.zip")," ", paste0(outputDir, "out.fa")))
          # download
          incProgress(1/5, detail = "Downloading FASTA...")
          file.copy(paste0(outputDir, "out.fa.zip"), file)
          # remove folder after use...
          system(paste0("rm -rf ",outputDir))
        })
      }
    }
  )
  
  # show seq vars info...
  output$seq_vars_count <- renderText({
    if (!is.null(data$SeqVars)){
      if (!"samples" %in% colnames(data$SeqVars)){
        return(paste0(data$SeqVars$vars, " sequence variant(s) found "))
      } else {
        return(paste0(nrow(data$SeqVars), " sequence variant(s) found "))
      }
    }
  })
  
}