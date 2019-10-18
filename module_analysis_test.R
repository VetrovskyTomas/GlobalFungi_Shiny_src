# Function for module UI
analysisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "Analyze sequences!"),
    
    
      # Sidebar panel for inputs ----
      sidebarPanel(width = 12,
        
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste you sequence", 
          "CAACCCTCAAGCTCTGCTTGGTATTGGGCTACACCCGACTGGGTGGGCCTTAAAATCAGTGGCGGTGCCATCTGGCTCTAAGCGTAGTAATTCTTCTCGCTCTGGAGATCTAGGTGTTTGCTTGTCAGCAACCCCCAATTTATCAAA", 
          width="100%",
          height = "200px"),
        
        
        hr(),
        
        actionButton(ns("buttSubmitSeq"), label = "Analyze"),
        hr(),
        actionButton(ns("buttGetBlastResult"), label = "Get blast result"),
        hr(),
        tableOutput(ns('info_table'))
      )
    )
  
}

# Function for module server logic
analysisFunc <- function(input, output, session, parent) {
  #namespace for dynamic input...
  ns <- session$ns
  
  # path 
  path <- "/home/fungal/databases/user_outputs/"
  
  # When user clicks on submit button : Update result tab...
  observeEvent(input$buttSubmitSeq, {
    # save the sequence and blast
    print(paste("you clicked on the button....",input$textSeq))
    
    # this should create unique folders for each upload. But this is breaking the app.
    # the solution should be in the "functions in progress" folder in the file "call_function_from_the_server_to_process_submitted_sequences.R"
    # outputDir <- paste0("responses_", as.integer(Sys.time())) 
    # system(paste("mkdir ", "/home/fungal/databases/user_outputs/",outputDir, sep = ""))
    
    input_fasta <- data.frame(seq1 = c(">query_seq", input$textSeq), stringsAsFactors = F)
    write.table(x = input_fasta,
                file = paste(path,"/my_query.fasta", sep = ""),
                quote = F, col.names = F, row.names = F)
    #cmd_blast <- paste0("blastn -db /home/fungal/databases/blast_database/my_self_made_db.fasta -query ",path, outputDir, "/my_query.fasta -out ", path, outputDir,"/results.out -outfmt 6")
    cmd_blast <- paste0("blastn -db /home/fungal/databases/blast_database/my_self_made_db.fasta -query ",path, "my_query.fasta -out ", path,"results.out -outfmt 6")
    system(cmd_blast)
  })
  
  observeEvent(input$buttGetBlastResult, {
    # Check if your blast finished ## please lets improve this to something more cleaver ##
    if(!dir.exists(path)){
      output$info_table <- renderTable({
        data.frame(blast=c("output1", "output2"), query=c("input1", "input2"), no_blast = c("no blast results", "not here..."), stringsAsFactors = F)
      })
    } else {
      output$info_table <- renderTable({
        blast_out <- read.delim(file = paste0(path, "results.out"), header = F)
        blast_out
      })

      }
  })
  
  # output$info_table <- renderTable({
  #   # blast result
  #   #data.frame(blast=c("output1", "output2"), query=c("input1", "input2"), stringsAsFactors = F)
  #   if(!file.exists(paste0(path, "results.out"))){
  #     data.frame(blast=c("output1", "output2"), query=c("input1", "input2"), no_blast = c("blast didn't run", "not here..."), stringsAsFactors = F)
  #     } else {
  #       blast_out <- read.delim(file = paste0(path, "results.out"), header = F)
  #       blast_out
  #     }
  # })
  
}
