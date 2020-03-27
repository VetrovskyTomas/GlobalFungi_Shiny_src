# Function for module UI
insertMetadataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6, 
        br(),
        br(),
        downloadButton(ns("buttTemplate"), label = "Download metadata template (xlsx)")
      ),
      column(6,
        br(),
        fileInput(ns('fileXLSX'), 'Insert filled template table (xlsx file)',accept = c(".xlsx"))
      )
    ),
    fluidRow(
      column(3, actionButton(ns("buttStart"), label = "Process metadata", icon = icon("microchip"))),
      column(9, verbatimTextOutput(ns('info_errors')))
    )
  )
}

# server logic to read selected file ----
insertMetadataFunc <- function(input, output, session, study) {

  ns <- session$ns
  
  output$buttTemplate <- downloadHandler(
    filename = "TemplateMetadata.xlsx",
    content = function(file) {
    file.copy(paste0("TemplateMetadata.xlsx"), file)
  })
  
  observeEvent(input$buttStart, {
    metatable <- as.data.frame(read_excel(input$fileXLSX$datapath, 1))
    if (nrow(metatable)>0){
      info = ""
      print(length(metatable[,1]))
      
      for (x in c(1:19)) {
        #print(colnames(metatable[x]))
        #print(length(metatable[,x]))
        i = 0
        for (y in metatable[,x]) {
          #print(y)
          i = i + 1
          if (is.na(y) ||toString(y) == ""){
            info = paste0(info, "EMPTY VALUE in column",colnames(metatable[x]),"(row ",i,")\n")
          }
        }
          
      }
    
      if (info != "") {
        alert(paste0(info))
        study$info <- info
        
        output$info_errors <- renderText(
          info
        )
      } else {
        print("You processed the metadata...")
        study$info <- "You processed the metadata..."
        
        study$metadata$data <- metatable
        
        study$metadata$test <- "OK"
        
        output$info_errors <- renderText(
          study$metadata$test
        )
      }
    } else {
      alert(paste0("Error: Table is empty!"))
    }
  })
}

