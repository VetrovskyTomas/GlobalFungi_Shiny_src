# Function for module UI with SH
resutsVariantsUI <- function(id) {
  ns <- NS(id)
  
  # show variants ui
    column(2,
           textOutput(ns("seq_vars_count")),
           downloadButton(ns("downloadFASTA"), "Download FASTA")
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
  
  # downlooad sequence variants FASTA
  output$downloadFASTA <- downloadHandler(
    filename = paste0(type,"_",text,".zip"),
    content = function(file) {
      file.copy(paste0(global_variants_path, type,"_",text,".zip"), file)
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