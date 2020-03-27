# Function for module UI
insertUploadUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(9, fileInput(ns('fileupload'),label = "Upload selected files", multiple = TRUE)),
      column(3, br(), actionButton(ns('reset'), 'Reset Input', icon = icon("broom")))
    ),
    fluidRow(column(12, actionButton(ns("buttStart"), label = "Process metadata", icon = icon("microchip")))),
    br(),
    fluidRow(
      column(12,"Uploaded file(s):"),
      column(12,verbatimTextOutput(ns('summary')))
    )
  )
}

# server logic to read selected file ----
insertUploadFunc <- function(input, output, session, study) {
  
  # namespace for dynamic input...
  ns <- session$ns
  
  options(shiny.maxRequestSize=10000*1024^2)
  
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
    return(paste0(file_input()$name," ", file_input()$datapath, " ", round(file_input()$size/1000000, digits = 2), " MB\n"))
  })  
  
  observeEvent(input$buttStart, {
    if (nrow(file_input()) == 0) {
      alert(paste0("Missing value - ",study_input()))
      study$info <- paste0("Missing value - ",check_inputs,"\n")
    } else {
      print("You processed the upload...")
      study$info <- paste0("Upload is finished for ",nrow(file_input())," files...")
      study$upload$data <- file_input()
      study$upload$test <- "OK"
    }
  })
  
}