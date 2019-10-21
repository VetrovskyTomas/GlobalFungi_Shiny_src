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
        # retrieve fasta using ... "samtools faidx"
        # generate folder for user task...
        outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
        system(paste("mkdir ", outputDir, sep = "")) #for linux
        # write fasta titles...
        input_titles <- data.frame(titles = data$SeqVars[,"hash"], stringsAsFactors = F)
        write.table(input_titles,file = paste0(outputDir,"my_titles.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)

        # # run command...
        cmd_blast <- paste0("xargs samtools faidx  /home/fungal/databases/blast_database/fm_sequences_vol1.fa < ",outputDir, "my_titles.txt > ", outputDir,"results.fa")
        system(cmd_blast)
        # linearize fasta...
        fasta_file <- fread(file = paste0(outputDir, "results.fa"), header = F)
        # remove folder after use...
        cmd_blast <- paste0("rm -rf ",outputDir)
        system(cmd_blast)
        ##################################################################
        # process fasta
        data$SeqVars$sequence <- vector(mode="character", length=nrow(data$SeqVars))
        name <- ""
        seq <- ""
        for (row in 1:nrow(fasta_file)) {
          line <- toString(fasta_file[row][1])
          first_char <- substring(line, 1, 1)
          if (first_char == ">"){
            if (seq != ""){
              data$SeqVars$sequence[data$SeqVars$hash == name] <- seq
            }
            name <- substring(line, 2)
            seq <- ""
          } else {
            seq <- paste0(seq,line)
          }
        }
        data$SeqVars$sequence[data$SeqVars$hash == name] <- seq
        #################################################
        D <- NULL
        # create fasta...
        if (input$seqs_derep == TRUE) {
          # dereplicated sequences...
          fasta <- data.frame(
            ids = seq.int(nrow(data$SeqVars)),
            seqs = data$SeqVars[,"sequence"]
          )
          fasta$ids <- sub("^", paste0(">",variable$type,"_",variable$text,"_"), fasta$ids)
          D <- do.call(rbind, lapply(seq(nrow(fasta)), function(i) t(fasta[i, ])))
        } else {
          # replicated sequences...
          s <- strsplit(data$SeqVars$samples, split = ";", fixed=TRUE)
          fasta <- data.frame(ids = unlist(s), seqs = rep(data$SeqVars$sequence, sapply(s, length)))
          fasta$indices = seq.int(nrow(fasta))
          fasta$titles <- paste0(fasta$indices,"_",fasta$ids)
          # drop unnecesary columns...
          fasta = subset(fasta, select = -c(indices,ids))
          # add > to titles...
          fasta$titles <- sub("^", paste0(">",variable$type,"_",variable$text,"_"),fasta$titles)
          fasta <- fasta[ , c("titles", "seqs")]
          D <- do.call(rbind, lapply(seq(nrow(fasta)), function(i) t(fasta[i, ])))
        }
        write.table(D, file, row.names = FALSE, col.names = FALSE, quote = FALSE)
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