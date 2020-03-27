# Function for module UI
insertUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='insert.png', height = 56)),
        column(11, h2(id="header_title", "Insert your study"))
      )
    ),
    # progress...
    sidebarPanel(id = ns("progress"), width = "100%", style = "background-color:white;",
      fluidRow(  
        column(6,verbatimTextOutput(ns('info_general'))),
        column(6, verbatimTextOutput(ns('info_progress')))
      )
    ),
    # content tabls
    sidebarPanel(id = ns("panel"), width = "100%", style = "background-color:white;",
      tabsetPanel(id = ns("tabs"),
        tabPanel("Introduction", value = "tab_intro",
          insertIntroUI(id = ns("insert_intro"))
        ),
        tabPanel("Basic info", value = "tab_basic",
          insertBasicUI(id = ns("insert_basic"))
        ),
        tabPanel("Metadata", value = "tab_metadata",
          insertMetadataUI(id = ns("insert_metadata"))
        ),
        tabPanel("Data upload", value = "tab_upload",
          insertUploadUI(id = ns("insert_upload"))
        ),
        tabPanel("Overview", value = "tab_overview",
            fluidPage(
              br(),
              fluidRow(
                #column(6,actionButton(ns('filechoose'),label = "Pick a file")),
                column(12, "ALL SEEMS TO BE CORRECT - NOW YOU CAN SUBMIT THE STUDY!")
              ),
              br(),
              fluidRow(  
                column(12,verbatimTextOutput(ns('info_over')))
              ),
              br(),
              fluidRow(
                column(3,actionButton(ns("buttSubmit"), label = "Submit your study", icon = icon("microscope")))
              )
          )
        )
      )
    )
  )
}

# server logic to read selected file ----
insertFunc <- function(input, output, session) {
  #
  ns <- session$ns
  
  # create storage for study info
  study <- reactiveValues()
  study$info <- NULL
  study$basic <- NULL
  study$metadata <- NULL
  study$correct <- FALSE

  # activate tables by steps...
  observe({
    # basic form
    if (!is.null(study$info)){
      print("Basic form is shown...")
      showTab(inputId = "tabs", target = "tab_basic")
      updateTabsetPanel(session, "tabs", selected = "tab_basic")
      ###############################################################
      # metadata form
      if (!is.null(study$basic)){
        print("Metadata form is shown...")
        showTab(inputId = "tabs", target = "tab_metadata")
        updateTabsetPanel(session, "tabs", selected = "tab_metadata")
        #*******************************************************************
        # upload form
        if (!is.null(study$metadata)){
          print("Upload form is shown...")
          showTab(inputId = "tabs", target = "tab_upload")
          updateTabsetPanel(session, "tabs", selected = "tab_upload")
          #---------------------------------------------
          if (!is.null(study$upload)){
            shinyjs::enable("buttSubmit")
            showTab(inputId = "tabs", target = "tab_overview")
            updateTabsetPanel(session, "tabs", selected = "tab_overview")
            # if (study$correct){
            #   study$info <- "DONE :)"
            # }
          }
          #---------------------------------------------
        }
        #*******************************************************************
      }
      ###############################################################
      } else {
      print("Forms are hidden...")
      hideTab(inputId = "tabs", target = "tab_basic")
      hideTab(inputId = "tabs", target = "tab_metadata")
      hideTab(inputId = "tabs", target = "tab_upload")
      hideTab(inputId = "tabs", target = "tab_overview")
      updateTabsetPanel(session, "tabs", selected = "tab_intro")
    }
  })
  
  observe({
    shinyjs::disable("buttSubmit")
    callModule(module = insertIntroFunc, id = "insert_intro", study)
    callModule(module = insertBasicFunc, id = "insert_basic", study)
    callModule(module = insertMetadataFunc, id = "insert_metadata", study)
    callModule(module = insertUploadFunc, id = "insert_upload", study)
  })
  
  # show filtered samples count info...
  output$info_general <- renderText({
    if (!is.null(study$info)){
      study$info
    } else {
      "Please read the instruction bellow."
    }
  })  
  
  # show progress...
  output$info_progress <- renderText({
    if (!is.null(study$basic)){
      info = "BASIC INFO"
      if (!is.null(study$metadata)){
        info = paste0(info,"BASIC INFO & METADATA")
        if (!is.null(study$upload)){
          info = paste0(info,"BASIC INFO & METADATA & UPLOAD")
        }
      }
      paste0(info," OK")
    } else {
      "BASIC INFO XXX"
    }
  })
  
  # try to submit the study...
  observeEvent(input$buttSubmit, {
    print("Try to submit the study...")
    
    # generate folder for user task...
    outputDir <- paste0(global_out_path,"study_", as.integer(Sys.time()),"/")
    system(paste("mkdir ", outputDir, sep = ""))
    
    # successful submition...
    output$info_general <- renderText({
      return(paste0("Your study was submited successfuly\n"))
    })
  })
  
  output$info_over <- renderText(
    paste0(
      "Title: ", study$basic$study_title, "\n",
      "Authors: ", study$basic$study_authors, "\n",
      "Year: ", study$basic$study_year, "\n",
      "Journal: ", study$basic$study_journal, "\n",
      "DOI: ", study$basic$study_doi, "\n",
      "Contributor: ", study$basic$study_contributor, "\n",
      "E-mail: ", study$basic$study_email, "\n",
      "Affiliation: ", study$basic$study_affiliation, "\n",
      "------------------------------------------------------------------------------------\n",
      "Samples: ", nrow(study$metadata$data), "\n",
      "------------------------------------------------------------------------------------\n",
      "Files: ", nrow(study$upload$data), "\n")
  )  
}
