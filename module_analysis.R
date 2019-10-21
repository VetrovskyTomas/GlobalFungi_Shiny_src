# Function for module UI
analysisUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "Analyze sequences!"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(width = 12,
        
        #text area for pasting the sequence...
        textAreaInput(ns("textSeq"), 
          "Paste you sequence", 
          "CAACCCTCAAGCTCTGCTTGGTATTGGGCTACACCCGACTGGGTGGGCCTTAAAATCAGTGGCGGTGCCATCTGGCTCTAAGCGTAGTAATTCTTCTCGCTCTGGAGATCTAGGTGTTTGCTTGTCAGCAACCCCCAATTTATCAAA", 
          width="100%",
          height = "200px"),
        
        # Input: Select a file ----
        # fileInput("file1", "Choose FASTA file",
        # multiple = FALSE,
        # accept = c("text/csv",
        # "text/comma-separated-values,text/plain",
        # ".csv")),
    
        hr(),
        
        #selectors
        fluidRow(
          column(4, checkboxInput(ns("is_extracted"), "ITS is already extracted", TRUE)),
          column(8, uiOutput(ns('dynamicInput_ITS')))
        ),
        column(8,selectInput(ns("data_type"), "Type:", choices = c("Exact", "SH"))),
        hr(),
        actionButton(ns("buttSubmitSeq"), label = "Analyze")
      ),
      # Main panel for outputs ----
      mainPanel(
        fluidRow(
          column(12, textOutput(ns("out_info")))
        )
      )
    )
  )
}

# Function for module server logic
analysisFunc <- function(input, output, session, parent) {
  # to be shared...
  vals <- reactiveValues()
  
  # When user clicks on submit button : Update result tab...
  observeEvent(input$buttSubmitSeq, {
    #info about result type...
    vals$type =  as.character("sequence")
    #info for output page...
    vals$text <- input$textSeq
      #paste("Sequences analyzed - ", "is ITS extracted: ",as.character(input$is_extracted), " / region : ", as.character(input$data_its)," / result type: ", as.character(input$data_type))
    
    callModule(session = parent, module = outputFunc, id = "id_results",vals, parent = parent)    
    updateTabItems(session = parent, "menu_tabs", "fmd_results")

  })
  
  #namespace for dynamic input...
  ns <- session$ns
  
  # The dynamic input definition
  output$dynamicInput_ITS <- renderUI({
  if (input$is_extracted == FALSE) {
    fluidRow(
      column(4,selectInput(ns("data_its"), "ITS:", choices = c("ITS1", "ITS2")))
    )
  } else {
    return(NULL)
  }
  })
  
}
