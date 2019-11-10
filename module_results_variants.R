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

  type <- gsub(" ", "_", type)
  text <- gsub(" ", "_", text)
  
  if (text == ""){
    text <- "variant"
  }
  
  if (type == ""){
    type <- "sequence"
  }
  
  # Downloadable fasta of selected sequence variants...
  output$downloadSeqVars <- downloadHandler(
    filename = "sequences.gz",
    content = function(file) {
      if (!is.null(data$SeqVars)){
        ##################################################
        print(print(paste0("Retrieve FASTA... exists global_fasta_out: ",exists("global_fasta_out"))))
        #if (!exists("global_fasta_out")){
        withProgress(message = 'Retrieving sequences...', {
          incProgress(1/7)
          # generate folder for user task...
          outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
          system(paste("mkdir ", outputDir, sep = "")) #for linux
          incProgress(1/7)
          # write fasta titles...
          input_titles <- data.frame(titles = data$SeqVars[,"hash"], stringsAsFactors = F)
          write.table(input_titles,file = paste0(outputDir,"my_titles.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
          incProgress(1/7)
          # # run command...
          cmd_blast <- paste0("blastdbcmd -db ", global_blast_db, " -line_length 2000 -entry_batch ", outputDir, "my_titles.txt > ", outputDir,"results.fa")
          system(cmd_blast)
          incProgress(1/7)
          # linearize fasta...
          fasta_file <- scan(file = paste0(outputDir, "results.fa"), character(), quote = "")
        
          # write samples table...
          write.table(data$SeqVars,file = paste0(outputDir,"my_samples.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)  
          
          # python
          system(paste0("python ", global_vars_to_fasta_py," ", outputDir,"results.fa ",outputDir,"my_samples.txt ",input$seqs_derep," ",type," ",text," ",outputDir,"out.fa"))
          
          incProgress(1/7, detail = "commpress and download") 
          system(paste0("gzip ", paste0(outputDir, "out.fa")))
          
          file.copy(paste0(outputDir, "out.fa.gz"), file)
          # remove folder after use...
          system(paste0("rm -rf ",outputDir))
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