# Function for module UI
insertUploadUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    br(),
    fluidRow(
      column(1, style = "background-color:white;",img(src='upload.png', height = 64)),
      column(8, fileInput(ns('fileupload'),label = "Upload selected files", multiple = TRUE)),
      column(3, br(), actionButton(ns('reset'), 'Reset Input', icon = icon("broom")))
    ),
    fluidRow(
      column(12,"File(s) prepared for upload:"),
      column(12,verbatimTextOutput(ns('summary')))
    ),
    br(),
    fluidRow(column(12, actionButton(ns("buttStart"), label = "Upload sequence data", icon = icon("microchip")))),
    br()
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
    shinyjs::disable("buttStart")
    values$upload_state <- 'reset'
  })
  
  observe({
    if (is.null(study$files)){
      shinyjs::disable("buttStart")
    } else {
      shinyjs::enable("buttStart")
    }
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
    #return(paste0(file_input()$name," ", file_input()$datapath, " ", round(file_input()$size/1000000, digits = 2), " MB\n"))
    if (!is.null(file_input()$name)){
      return(paste0(file_input()$name," ", round(file_input()$size/1000000, digits = 2), " MB\n"))
    }
  })  
  
  observeEvent(input$buttStart, {
    if (nrow(file_input()) == 0) {
      alert(paste0("Missing value - ",study_input()))
      study$info <- paste0("Missing value - ",check_inputs,"\n")
    } else {
      print("You processed the upload...")
      #*******************************************
      # generate folder for study data...
      
      outputDir <- paste0(global_out_path, study$key, "/")
      print(outputDir)
      system(paste("mkdir ", outputDir, sep = ""))
      
      for (x in c(1:length(file_input()$datapath))) {
        cmd <- paste0("mv ",file_input()$datapath[x]," ",outputDir, file_input()$name[x])
        print(cmd)
        system(cmd)
      }
      #*******************************************
      study$info <- paste0("Upload is finished for ",nrow(file_input())," files...")
      study$upload$data <- file_input()
      study$upload$test <- "OK"
    }
  })
  
}