# Function for module UI
insertUploadUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      #column(6,actionButton(ns('filechoose'),label = "Pick a file")),
      column(9, fileInput(ns('fileupload'),label = "Upload selected files", multiple = TRUE)),
      column(3, br(), actionButton(ns('reset'), 'Reset Input'))
    ),
    fluidRow(
      column(12,verbatimTextOutput(ns('summary')))
    ),
    fluidRow(
      column(12,tableOutput(ns('files')))
    )
  )
}

# server logic to read selected file ----
insertUploadFunc <- function(input, output, session, study) {
  
  # namespace for dynamic input...
  ns <- session$ns
  
  options(shiny.maxRequestSize=10000*1024^2)
  
  # path <- reactiveValues(
  #   pth=NULL
  # )
  # 
  # observeEvent(input$filechoose,{
  #   path$pth <- rbind(path$pth, file.choose())
  # })  
    
  values <- reactiveValues(
    upload_state = NULL
  )  
  
  observeEvent(input$fileupload, {
    values$upload_state <- 'uploaded'
    inFile <- input$fileupload
    if (is.null(inFile))
      return()

    if (is.null(study$files)){
      study$files <- inFile
    } else {
      study$files <- rbind(study$files, inFile)
    }
  })
    
  observeEvent(input$reset, {
    reset('fileupload')
    values$upload_state <- 'reset'
  })
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$fileupload)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$summary <- renderText({
    return(paste("Uploaded file:", file_input()$name))
  })  
  
  output$files <- renderTable({
    if (!is.null(study$files)){
      study$files
    }
  })
  
}