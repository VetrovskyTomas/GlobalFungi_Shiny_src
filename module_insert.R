# Function for module UI
insertUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  
  fluidPage(
    # App title ----
    h1(id="welcome_title", "Insert your study"),
    sidebarPanel(width = "100%", style = "background-color:white;",
      fluidRow(
        column(9,   
          tabsetPanel(id = ns("navbar"),
            tabPanel("How to submit your study!",
              br(),
              wellPanel(
                fluidRow(
                  h2("First step...")
                )
              )
              ),
            tabPanel("1. Study info",
              br(),
              wellPanel(
              fluidRow(
                h2("Study info"),
                fluidRow(
                  column(12, "Please fill all the fields...")
                ),
                br(),
                textInput(inputId = ns("study_title"), label = "Title", width = "100%"),
                textInput(inputId = ns("study_authors"), label = "All authors", width = "300px", placeholder = "e.g.: Vetrovsky, T., Baldrian, P. and Morais, D."),
                textInput(inputId = ns("study_year"), label = "Year of publication", width = "300px"),
                textInput(inputId = ns("study_journal"), label = "Journal", width = "300px"),
                textInput(inputId = ns("study_doi"), label = "DOI of the paper", width = "300px"),
                textInput(inputId = ns("study_contributor"), label = "Contributor Name", width = "300px"),
                textInput(inputId = ns("study_email"), label = "Contributor E-mail", width = "300px"),
                textInput(inputId = ns("study_affiliation"), label = "Contributor Affiliation", width = "300px")
                )
              )
              ),
            tabPanel("2. Samples metadata",
              br(),
              wellPanel(
                fluidRow(
                  downloadButton(ns("buttTemplate"), label = "Download metadata template (xlsx)")
                ),
                fluidRow(
                  column(12, "Please use dot . as floating point!"),
                  tableOutput(ns('instructions_table')),
                  fileInput(ns('fileXLSX'), 'Choose xlsx file',accept = c(".xlsx"))
                ),
                fluidRow(
                  tableOutput(ns('contents'))
                )
              )
            ),
            tabPanel("3. Data upload",
              br(),               
              wellPanel(
                fluidRow(
                  fileInput(ns('filesFASTQ'), 'Choose FASTQ files', accept = c(".FASTQ",".fastq",".fq"), multiple = TRUE)
                ),
                fluidRow(
                  #verbatimTextOutput(ns('upload_content'))
                  tableOutput(ns("files"))
                )
              )
            )
          )
        ),
        column(3,  
          uiOutput(ns('dynamic_info'))
        )
      )
    )
  )
}

# Define server logic to read selected file ----
insertFunc <- function(input, output, session) {
  
  # namespace for dynamic input...
  ns <- session$ns
  
  #
  options(shiny.maxRequestSize=100*1024^2)
 
  #
  output$contents <- renderTable({
    req(input$fileXLSX)
    inFile <- input$fileXLSX
    read_excel(inFile$datapath, 1)
  })
  
  # #
  # fastqs<-reactive({
  #   rbindlist(lapply(input$filesFASTQ$datapath, fread),
  #             use.names = TRUE, fill = TRUE)
  # })
  # #
  # observe({
  #   output$upload_content <- renderText(nrow(fastqs()))
  #   print(paste0("Files uploaded ",nrow(fastqs())))
  # })
  
  # output$upload_content <- renderTable({
  #   req(input$filesFASTQ)
  #   print(rbindlist(lapply(input$filesFASTQ$datapath, fread),
  #             use.names = TRUE, fill = TRUE))
  # })
  
  # observeEvent(input$filesFASTQ, {
  #   inFile <- input$filesFASTQ
  #   if (is.null(inFile))
  #     return()
  #   file.copy(inFile$datapath, file.path("c:/Temp", inFile$name) )
  # })
  
  output$files <- renderTable({
    input$filesFASTQ
    })
  
  #
  output$instructions_table <- renderTable(insert_instructions_table)
  
  output$buttTemplate <- downloadHandler(
    filename = "TemplateMetadata.xlsx",
    content = function(file) {
      file.copy(paste0("TemplateMetadata.xlsx"), file)
    })
  
  # check if inputs are correct...
  study_input <- reactive({
    if (input$study_title == "") {
      return("Title field is empty!")
    } else if (input$study_authors == "") {
      return("Authors field is empty!")
    } else if (input$study_year == "") {
      return("Year field is empty!")
    } else if (input$study_journal == "") {
      return("Journal field is empty!")
    } else if (input$study_doi == "") {
      return("DOI field is empty!")
    } else if (input$study_contributor == "") {
      return("Contributor field is empty!")
    } else if (input$study_email == "") {
      return("Email field is empty!")
    } else if (input$study_affiliation == "") {
      return("Affiliation field is empty!")
    }
    return("")
  })
  
  # try to submit the study...
  observeEvent(input$buttSubmit, {
    print("Try to submit the study...")
    print(paste0("Try to submit the study... ",input$study_title))
    
    # check if inputs are correct...
    if (study_input() != "") {
      alert(paste0("Missing value in study form - ",study_input()))
      output$info_check <- renderText({
        return(paste0("Missing value in study form - ",study_input(),"\n"))
      })
      return(NULL)
    }
    
    # generate folder for user task...
    outputDir <- paste0(global_out_path,"study_", as.integer(Sys.time()),"/")
    system(paste("mkdir ", outputDir, sep = ""))
    
    # successful submition...
    output$info_check <- renderText({
      return(paste0("Your study was submited successfuly\n"))
    })
    
  })

  # dynamic colorful panel...
  #observe({
    output$dynamic_info <- renderUI({
      if (study_input() != "") {
        wellPanel(id = "w1",
        tags$style("#w1 {background-color:#c90000;}"),
        verbatimTextOutput(ns('info_check')),
        h2("Check & Submit"),
        actionButton(ns("buttSubmit"), label = "Submit")
        )
      } else {
        wellPanel(id = "w1",
        tags$style("#w1 {background-color:#00a30c;}"),
        verbatimTextOutput(ns('info_check')),
        h2("Check & Submit"),
        actionButton(ns("buttSubmit"), label = "Submit")
        )
      }
    })
  #})
}
