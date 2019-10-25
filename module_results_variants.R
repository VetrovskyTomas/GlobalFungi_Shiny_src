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
resutsVariantsFunc <- function(input, output, session,  data, type, text) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  # Downloadable fasta of selected sequence variants...
  output$downloadSeqVars <- downloadHandler(
    filename = "sequences.zip",
    content = function(file) {
      if (!is.null(data$SeqVars)){
        ##################################################
        print(print(paste0("Retrieve FASTA... exists global_fasta_out: ",exists("global_fasta_out"))))
        if (!exists("global_fasta_out")){
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

          # remove folder after use...
          cmd_blast <- paste0("rm -rf ",outputDir)
          system(cmd_blast)
          incProgress(1/5)
        })
        } else {
          withProgress(message = 'Loading...', {
            # write fasta titles...
            #input_titles <- data.frame(titles = data$SeqVars[,"hash"], stringsAsFactors = F)
            #write.table(input_titles,file = "C:/fm_database_root/tables/my_titles.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
            for(i in 1:5) {
              incProgress(1/5)
              Sys.sleep(0.2)
            }
          })
          fasta_file <- global_fasta_out
        }
        ##################################################################
        withProgress(message = 'Processing...', {
          n <- fasta_file[seq(1, length(fasta_file), 2)]
          n <- sub('.', '', n)
          m <- fasta_file[seq(2, length(fasta_file), 2)]
          df_fasta <- data.frame(hash=n, seqs=m, stringsAsFactors = F)
          data$SeqVars$sequence <- df_fasta$seqs[match(data$SeqVars$hash, df_fasta$hash)]
        })
        #################################################
        withProgress(message = 'Formating...', {
          fasta <- NULL
          # create fasta...
          if (input$seqs_derep == TRUE) {
            # dereplicated sequences...
            fasta <- data.frame(
              ids = seq.int(nrow(data$SeqVars)),
              type = data$SeqVars$marker,
              seqs = data$SeqVars$sequence
            )
            incProgress(1/5, detail = "preparing titles")
            fasta$titles <- paste0(fasta$type,"_",fasta$ids)
            incProgress(1/5, detail = "soing sub")
            fasta$titles <- sub("^", paste0(type,"_",text,"_"),fasta$titles, perl = TRUE)
            incProgress(1/5, detail = "creating FASTA - rbind")
            fasta <- fasta[ , c("titles", "seqs")]
          } else {
            # replicated sequences...
            s <- strsplit(data$SeqVars$samples, split = ";", fixed=TRUE)
            fasta <- data.frame(ids = unlist(s), seqs = rep(data$SeqVars$sequence, sapply(s, length)), type = rep(data$SeqVars$marker, sapply(s, length)))
            fasta$indices = seq.int(nrow(fasta))
            incProgress(1/5, detail = "preparing titles")
            fasta$titles <- paste0(fasta$type,"_",fasta$indices,"_",fasta$ids)
            incProgress(1/5, detail = "soing sub")
            # drop unnecesary columns...
            fasta = subset(fasta, select = -c(indices,ids))
            # add > to titles...
            fasta$titles <- sub("^", paste0(type,"_",text,"_"),fasta$titles, perl = TRUE)
            incProgress(1/5, detail = "creating FASTA - rbind")
            fasta <- fasta[ , c("titles", "seqs")]
            
          }
        incProgress(1/5, detail = "writing...")
        seq_file <- paste0("sequences_", as.integer(Sys.time()),".fa")
        dataframe2fas(fasta, seq_file)
        
        incProgress(1/5, detail = "commpress and download") 
        zip(zipfile=file, files=seq_file)
        
        # delete file...
        system(paste0("rm ",seq_file))
        })
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