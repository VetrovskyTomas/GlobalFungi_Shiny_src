# Function for module UI with SH
resutsVariantsUI <- function(id) {
  ns <- NS(id)
  
  # show variants ui
    column(2,
           textOutput(ns("seq_vars_count")),
           downloadButton(ns("downloadSeqVars"), "Download FASTA"),
           checkboxInput(ns("seqs_derep"), "dereplicated", TRUE)
    )
}

# Function for module server logic
resutsVariantsFunc <- function(input, output, session,  data, variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  # Downloadable fasta of selected sequence variants...
  output$downloadSeqVars <- downloadHandler(
    filename = "sequences.fasta",
    content = function(file) {
      if (!is.null(data$SeqVars)){
        ##################################################
        withProgress(message = 'Retrieving sequences...', {
          incProgress(1/5)
          # generate folder for user task...
          outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
          system(paste("mkdir ", outputDir, sep = "")) #for linux
          incProgress(1/5)
          # write fasta titles...
          input_titles <- data.frame(titles = data$SeqVars[,"hash"], stringsAsFactors = F)
          write.table(input_titles,file = paste0(outputDir,"my_titles.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
          incProgress(1/5)
          # # run command...
          cmd_blast <- paste0("blastdbcmd -db  /home/fungal/databases/blast_database/fm_sequences_vol1.fa -line_length 2000 -entry_batch ",outputDir, "my_titles.txt > ", outputDir,"results.fa")
          system(cmd_blast)
          incProgress(1/5)
          # linearize fasta...
          fasta_file <- scan(file = paste0(outputDir, "results.fa"), character(), quote = "")
          #fasta_file <- scan("C:/fm_database_root/tables/results.fa", character(), quote = "")
          
          # remove folder after use...
          cmd_blast <- paste0("rm -rf ",outputDir)
          system(cmd_blast)
          incProgress(1/5)
        })
        ##################################################################
        withProgress(message = 'Processing...', {
          data$SeqVars$sequence <- vector(mode="character", length=nrow(data$SeqVars))
          n <- fasta_file[seq(1, length(fasta_file), 2)]
          n <- sub('.', '', n)
          m <- fasta_file[seq(2, length(fasta_file), 2)]
          df_fasta <- data.frame(hash=n, seqs=m, stringsAsFactors = F)
          data$SeqVars$sequence[data$SeqVars$hash %in% df_fasta$hash] <- df_fasta$seqs
        })
        #################################################
        D <- NULL
        withProgress(message = 'Formating...', {
          # create fasta...
          if (input$seqs_derep == TRUE) {
            # dereplicated sequences...
            fasta <- data.frame(
              ids = seq.int(nrow(data$SeqVars)),
              type = data$SeqVars$marker,
              seqs = data$SeqVars$sequence
            )
            fasta$titles <- paste0(fasta$type,"_",fasta$ids)
            fasta$titles <- sub("^", paste0(">",variable$type,"_",variable$text,"_"),fasta$titles)
            fasta <- fasta[ , c("titles", "seqs")]
            D <- do.call(rbind, lapply(seq(nrow(fasta)), function(i) t(fasta[i, ])))
          } else {
            # replicated sequences...
            s <- strsplit(data$SeqVars$samples, split = ";", fixed=TRUE)
            fasta <- data.frame(ids = unlist(s), seqs = rep(data$SeqVars$sequence, sapply(s, length)), type = rep(data$SeqVars$marker, sapply(s, length)))
            fasta$indices = seq.int(nrow(fasta))
            fasta$titles <- paste0(fasta$type,"_",fasta$indices,"_",fasta$ids)
            # drop unnecesary columns...
            fasta = subset(fasta, select = -c(indices,ids))
            # add > to titles...
            fasta$titles <- sub("^", paste0(">",variable$type,"_",variable$text,"_"),fasta$titles)
            fasta <- fasta[ , c("titles", "seqs")]
            D <- do.call(rbind, lapply(seq(nrow(fasta)), function(i) t(fasta[i, ])))
          }
        })
        write.table(D, file, row.names = FALSE, col.names = FALSE, quote = FALSE)
        #write.table(fasta_file, file, row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
    }
  )
  
  # show seq vars info...
  output$seq_vars_count <- renderText({
    if (!is.null(data$SeqVars)){
      return(paste0("Found ", nrow(data$SeqVars), " of sequnce variants "))
    }
  })
}