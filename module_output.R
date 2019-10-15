# Function for module UI
outputUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # title...
    h2(id="welcome_title",textOutput(ns("out_type"))),
    # paper info table...
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
       verbatimTextOutput(ns('info_key')),
       fluidRow(
       column(10,tableOutput(ns('info_table'))),
       uiOutput(ns('dynamic_button'))
       )
    ),
    uiOutput(ns('dynamic_content'))
  )
  
}

# Function for module server logic
outputFunc <- function(input, output, session, variable, parent) {
  # namespace for dynamic input...
  ns <- session$ns
  
  # to be shared...
  out_data <- reactiveValues()

  # server logis for dynamic content based on result type...
  observe({
    
    # default
    out_data$samples <- NULL
    out_data$SHs <- NULL
    out_data$SeqVars <- NULL
    
    if (variable$text != "") {
    # study option...
    if (variable$type == "study"){
      # filter papers data...
      output$info_table <- renderTable({
        global_papers[which(global_papers$paper_id %in% variable$text),c("title", "authors", "year","journal", "doi", "contact")]
      })
      # filer sample based on selection...
      out_data$samples <- global_samples[which(global_samples$paper_id %in% variable$text), c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    } else
    # sequence option...  
    if (variable$type == "sequence") {
      # filer sample based on selection...
      variants <- global_variants[which(global_variants$sequence %in% variable$text), c("sequence", "samples", "abundances", "SH")]
      #print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
      if (nrow(variants) > 0){
        # get info table...
        if (variants$SH != "-"){
          output$info_table <- renderTable({
            global_SH[which(global_SH$SH %in% variants$SH),]
          })
        }
        else
        {
          output$info_table <- renderTable({
            bar_data <- data.frame(
              SH = c("-")
            )
          })
        }
        samples <- strsplit(variants$samples, ';', fixed=TRUE)
        samples <- unique(unlist(samples))
        print(paste0("Number of samples in ",variable$text," is ", length(samples)))
          
        out_data$samples <- global_samples[which(global_samples$id %in% samples), c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
      }
      else
      {
        output$info_table <- renderTable({
          bar_data <- data.frame(
            status = c("No hit!")
          )
        })
      }
    } else
    # SH option...  
    if (variable$type == "SH") {
      # filter papers data...
      output$info_table <- renderTable({
        global_SH[which(global_SH$SH %in% variable$text),]
      })
      # filer sample based on selection...
      variants <- global_variants[which(global_variants$SH %in% variable$text), c("sequence", "samples", "abundances")]
      print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
      out_data$SeqVars <- variants[,c("samples","sequence")]
      
      samples <- strsplit(variants$samples, ';', fixed=TRUE)
      samples <- unique(unlist(samples))
      print(paste0("Number of samples in ",variable$text," is ", length(samples)))
      
      out_data$samples <- global_samples[which(global_samples$id %in% samples), c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    } else
    # species option...  
    if (variable$type == "species") {
      # get all SH based on species...
      SH_list <- global_SH[which(global_SH$Species %in% variable$text),]
      print(paste0("Number of SH in ",variable$text," is ", nrow(SH_list)))
      # pass the list of SHs...
      out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
      # filter species data...
      output$info_table <- renderTable({
        tax_tab <- out_data$SHs[1,c("Species","Kingdom", "Phylum", "Class", "Order", "Family", "Genus")]
      })
      # filer sample based on selection...
      variants <- global_variants[which(global_variants$SH %in% SH_list$SH), c("sequence", "samples", "abundances")]
      print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
      out_data$SeqVars <- variants[,c("samples","sequence")]
        
      samples <- strsplit(variants$samples, ';', fixed=TRUE)
      samples <- unique(unlist(samples))
      print(paste0("Number of samples in ",variable$text," is ", length(samples)))
        
      out_data$samples <- global_samples[which(global_samples$id %in% samples), c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    } else
    # genus option...  
    if (variable$type == "genus") {
      # get all SH based on genus...
      SH_list <- global_SH[which(global_SH$Genus %in% variable$text),]
      print(paste0("Number of SH in ",variable$text," is ", nrow(SH_list)))
      # pass the list of SHs...
      out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
      # filter genus data...
      output$info_table <- renderTable({
        tax_tab <- out_data$SHs[1,c("Genus","Kingdom", "Phylum", "Class", "Order", "Family")]
      })
      # filer sample based on selection...
      variants <- global_variants[which(global_variants$SH %in% SH_list$SH), c("sequence", "samples", "abundances")]
      print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
      out_data$SeqVars <- variants[,c("samples","sequence")]
      
      samples <- strsplit(variants$samples, ';', fixed=TRUE)
      samples <- unique(unlist(samples))
      print(paste0("Number of samples in ",variable$text," is ", length(samples)))
      
      out_data$samples <- global_samples[which(global_samples$id %in% samples), c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    } 
    #############################################################
    # general output...
    if (is.null(out_data$samples)){
      #
    } else {
      callModule(module = outputGeneralFunc, id = "results_out", out_data, parent = parent)
    }
    }
  })
  
  # key
  output$info_key <- renderText(
    if (variable$type == "study"){
      toString(global_papers[which(global_papers$paper_id %in% variable$text),"title"])
    } else { 
      variable$text
    }
  )
  ###########################################################################
  
  # dynamic content based on result type...
  output$dynamic_content <- renderUI({
    if (is.null(out_data$samples)){
        sidebarPanel(width = "100%", style = "background-color:white;",
          h2("No results found.")
        )
    } else {
      if (!is.null(out_data$SHs)){
        outputGeneralSHUI(id = ns("results_out"))
      } else
      {
        outputGeneralUI(id = ns("results_out"))
      }
    }
  })
  
  
    # dynamic button - seq vars...
    output$dynamic_button <- renderUI({
      if (!is.null(out_data$SeqVars)){
        column(2,
          textOutput(ns("seq_vars_count")),
          downloadButton(ns("downloadSeqVars"), "Download FASTA"),
          checkboxInput(ns("seqs_derep"), "dereplicated", TRUE)
        )
      }
    })
    
    # Downloadable fasta of selected sequence variants...
    output$downloadSeqVars <- downloadHandler(

      filename = "sequences.fasta",
      content = function(file) {
        if (!is.null(out_data$SeqVars)){
          D <- NULL
          # create fasta...
          if (input$seqs_derep == TRUE) {
            # dereplicated sequences...
            fasta <- data.frame(
              ids = seq.int(nrow(out_data$SeqVars)),
              seqs = out_data$SeqVars[,"sequence"]
            )
            fasta$ids <- sub("^", paste0(">",variable$type,"_",variable$text,"_"), fasta$ids)
            D <- do.call(rbind, lapply(seq(nrow(fasta)), function(i) t(fasta[i, ])))
          } else {
            # replicated sequences...
            s <- strsplit(out_data$SeqVars$samples, split = ";", fixed=TRUE)
            fasta <- data.frame(ids = unlist(s), seqs = rep(out_data$SeqVars$sequence, sapply(s, length)))
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
      if (!is.null(out_data$SeqVars)){
        return(paste0("Found ", nrow(out_data$SeqVars), " of sequnce variants "))
      }
    })

  
  # show output info...
  output$out_type <- renderText({
    num_samples <- 0
    if (!is.null(out_data$samples)){
      num_samples <- nrow(out_data$samples)
    }
    return(paste0("Here are the results for ", variable$type, " covering ", num_samples, " samples")) 
  })
  
}