# setwd(dir = "D:/Fungal_Metastudy_DB/")
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

# Interesting suggestions: https://github.com/daattali/advanced-shiny#busy--done--error-feedback-after-pressing-a-button

library(data.table)
library(shiny)
library(DT)

# Global
SNVs_Samples_counts_SHs_IDs <- fread(input = "C:/fm_database_root/tables/toy_SNV_samples_counts_SH_MD5SUM.txt", header = FALSE)
metadata <- fread(input = "C:/fm_database_root/tables/FUNGAL_METASTUDY_Filtered_metadata.txt")

# User interface
ui <- fluidPage(
  titlePanel("Meta-study fasta input"),
  # Create a new Row in the UI for selectInputs
  sidebarPanel(
    textAreaInput(inputId = "text",
                  label = "Fasta text",
                  width = "400px",
                  height = "400px"),
    # textInput(inputId = "text", 
    #           label = "Fasta text"),
    actionButton(inputId = "submit.text",
                 label = "Submit FASTA")
    # Select a fasta file  ----
    # fileInput(inputId = "file1", 
    #           label = "Choose FASTA File",
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv")),
    # actionButton("submit.upload", "Submit from file")
  ),
  
  # Create a panel and a set of tabs for the map and the table.
  mainPanel(
    DT::dataTableOutput("contents"),
    textOutput("nSeqs")
  ))

## server

server <- function(input, output, session) {
  # 
  # ## function to turn txt into link --------------
  # ## Solution taken from: https://stackoverflow.com/questions/42296816/shiny-turn-element-in-output-table-into-link
  # ToLink <- function(txt,link) {
  #   paste0('<a href=',link,">",txt,'</a>')
  # }
  
  # fasta_df <- reactive({
  #   req(input$text)
  #   })
  
  observeEvent(input$submit.text, {
    
    output$contents <- DT::renderDataTable({
      fasta_df <-  read.delim(textConnection(input$text), header = F)
      df_base_fasta <- data.frame(header = fasta_df[seq(1,nrow(fasta_df),2),], 
                                  bases = as.character(fasta_df[seq(2,nrow(fasta_df),2),]), 
                                  stringsAsFactors = F)
      # df_base_fasta
      hits_to_the_database <- SNVs_Samples_counts_SHs_IDs[chmatch(x = df_base_fasta$bases, table = SNVs_Samples_counts_SHs_IDs$V1), c("V2", "V4", "V5")]
      
      # The user sequences were searched against our database for perfect match. The table below returns the user's sequence header and the hit against our database.
      output_data <- data.frame(header = df_base_fasta$header,
                                Samples_from_Fungal_Metastudy_DB = hits_to_the_database$V2,
                                UNITE_07_Spec.Hypothesis = hits_to_the_database$V4,
                                MD5SUM_link = hits_to_the_database$V5, stringsAsFactors = F)
      # Include links
      output_data$UNITE_07_Spec.Hypothesis <- gsub(pattern = "SH", replacement = "http://147.231.45.225:443/?Spec.Hyp=SH", x = hits_to_the_database$V4)
      positions_to_link <- grep(pattern = "SH", x = output_data$UNITE_07_Spec.Hypothesis)
      output_data$UNITE_07_Spec.Hypothesis[positions_to_link] <- paste0('<a href=',output_data$UNITE_07_Spec.Hypothesis[positions_to_link],">",hits_to_the_database$V4[positions_to_link],'</a>')
      
      # Add no_hits
      output_data[is.na(output_data)] <- "no_hit"
      output_data
    }, escape = FALSE)
    output$nSeqs <- renderText({
      
      fasta_df <-  read.delim(textConnection(input$text), header = F)
      df_base_fasta <- data.frame(header = fasta_df[seq(1,nrow(fasta_df),2),], 
                                  bases = as.character(fasta_df[seq(2,nrow(fasta_df),2),]), 
                                  stringsAsFactors = F)
      # df_base_fasta
      hits_to_the_database <- SNVs_Samples_counts_SHs_IDs[chmatch(x = df_base_fasta$bases, table = SNVs_Samples_counts_SHs_IDs$V1), c("V2", "V4", "V5")]
      
      # The user sequences were searched against our database for perfect match. The table below returns the user's sequence header and the hit against our database.
      output_data <- data.frame(header = df_base_fasta$header,
                                Samples_from_FungalMTS_DB = hits_to_the_database$V2,
                                UNITE_07_Spec.Hypothesis = hits_to_the_database$V4,
                                MD5SUM_link = hits_to_the_database$V5, stringsAsFactors = F)
      # Include links
      output_data$UNITE_07_Spec.Hypothesis <- gsub(pattern = "SH", replacement = "http://147.231.45.225:443/?Spec.Hyp=SH", x = hits_to_the_database$V4)
      positions_to_link <- grep(pattern = "SH", x = output_data$UNITE_07_Spec.Hypothesis)
      output_data$UNITE_07_Spec.Hypothesis[positions_to_link] <- paste0('<a href=',output_data$UNITE_07_Spec.Hypothesis[positions_to_link],">",hits_to_the_database$V4[positions_to_link],'</a>')
      
      # Add no_hits
      output_data[is.na(output_data)] <- "no_hit"
      
      output_data
      paste0("You got ", length(grep("no_hit", output_data$Samples_from_FungalMTS_DB)), " \"no hit\" between your FASTA input and the database")
    })
  })
  
  # Chunk of code to upload file.
  
  # observeEvent(input$chck_file,{
  #   output$nSeqs <- renderText({
  #     if(file.exists(paste0(outputDir, "/out1.txt"))){
  #       n_of_seqs <- as.character(read.delim(file = paste0(outputDir, "/out1.txt"), 
  #                                            header = F, 
  #                                            stringsAsFactors = F))
  #       paste("File exists in: ", paste(getwd(), outputDir, sep = "/"), 
  #             sep="")
  #       paste("Fasta contains ", n_of_seqs, " sequences", sep = "")
  #     } else {
  #       paste("There are no files to count in: ",paste(getwd(), outputDir, sep = "/"), sep="")
  #     }
  #   })
  #   
  # })
  session$onSessionEnded(stopApp) #This interferes with the url generation usage. We need to discover why...
}

shinyApp(ui = ui, server = server, options = list(launch.browser = T))