# App for submitting fasta
# Storing the submitted fasta
# Running bash command on stored data 
# Show the user the outcome of the bash command.

## ui
library(shiny)

fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose FASTA File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("chck_file", "Check for file")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      textOutput("nSeqs")
    )
  )
)

## server

function(input, output) {
  
  # Generating folder for data creation
  outputDir <- paste0("responses_", as.integer(Sys.time()))
  #system(paste("mkdir ", outputDir, sep = "")) #for linux
  dir.create(outputDir) #for windows
  # Defining function to save data
  saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("%s_%s.fasta", "data1", as.integer(Sys.time()))
    # Write the file to the local system
    write.table(
      x = data,
      file = paste(outputDir, fileName, sep = "/"),
      quote = F, 
      sep = "\t",
      row.names = F,
      col.names = F
    )
  }
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.delim(input$file1$datapath,
                         header = F,
                         stringsAsFactors = F)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    #Saving data in the folder
    colnames(df)[] <- "Fasta file, 3 first sequences"
    saveData(df)
    fileName1 <- dir(path = outputDir)
    full_path_in <- paste(outputDir, fileName1, sep = "/")
    full_path_out <- paste0(outputDir, "/out1.txt")
    cmd <- paste("grep -c '>'", full_path_in, 
                 ">", full_path_out, 
                 sep = " ")
    system(cmd)
    
    #table formation
    return(head(df))
    
  })
  observeEvent(input$chck_file,{
    output$nSeqs <- renderText({
      if(file.exists(paste0(outputDir, "/out1.txt"))){
        n_of_seqs <- as.character(read.delim(file = paste0(outputDir, "/out1.txt"), 
                                             header = F, 
                                             stringsAsFactors = F))
        paste("File exists in: ", paste(getwd(), outputDir, sep = "/"), 
              sep="")
        paste("Fasta contains ", n_of_seqs, " sequences", sep = "")
      } else {
        paste("There are no files to count in: ",paste(getwd(), outputDir, sep = "/"), sep="")
      }
    })
    
  })
  
}