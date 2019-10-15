# Function for module UI
studiesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "All studies:"),
    sidebarPanel(width = "100%", style = "background-color:white;",
      DT::dataTableOutput(ns("data"))
    )
  )
  
}

# Function for module server logic
studiesFunc <- function(input, output, session, parent) {
  # to be shared...
  vals <- reactiveValues()  
  
  #namespace for dynamic input...
  ns <- session$ns
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  df <- reactiveValues(data = data.frame(
    Name =  global_papers$title,
    Authors = global_papers$authors,
    Journal = global_papers$journal,
    Year =  global_papers$year,
    DOI = global_papers$doi,
    Actions = shinyInput(actionButton, nrow(global_papers), 'button_', label = "Show", 
      onclick = paste0("Shiny.onInputChange('", ns("select_button"), "', this.id);",
                       "Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                       "Shiny.onInputChange('", ns("lastClick"), "', Math.random())")),
    stringsAsFactors = FALSE,
    row.names = 1:nrow(global_papers)
  ))
  
  output$data <- DT::renderDataTable(
    df$data, server = FALSE, escape = FALSE, selection = 'none'
  )

  # redirect...  
  observeEvent(input$lastClick, {
    #print(input$lastClickId)
    selectedRow <- as.numeric(strsplit(input$lastClickId, "_")[[1]][2])
    #info about result type...
    vals$type =  "study"
    #pass code of the study...
    vals$text <- toString(global_papers[selectedRow,1])
    callModule(session = parent, module = outputFunc, id = "id_results",vals)
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
    }
  )
  
}