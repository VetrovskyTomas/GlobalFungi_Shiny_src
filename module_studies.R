# Function for module UI
studiesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='studies.png', height = 56)),
                   column(11, h2(id="header_title", "Studies"))
                 )
    ),
    # content
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
  
  # sort papers
  #order_columns <- c("add_date", "authors")
  #rowidx <- order(global_papers[, order_columns[1]], global_papers[, order_columns[2]])
  #global_papers <- global_papers[rowidx, , drop = FALSE]
  
  # Sort the Global_papers data frame by year in ascending order
  global_papers <- global_papers[order(global_papers$year),]
  
  # fill data frame
  df <- reactiveValues(data = data.frame(
    Id = global_papers$id,
    Inserted = global_papers$add_date,
    Title = global_papers$title,
    Authors = global_papers$authors,
    Journal = global_papers$journal,
    Year = global_papers$year,
    DOI = global_papers$doi,
    manipulated = ifelse(global_papers$manipulated == 1, "Yes", "No"),
    Actions = shinyInput(actionButton, nrow(global_papers), 'button_', label = "Show", 
                         onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                          "Shiny.onInputChange('", ns("lastClick"), "', Math.random())")),
    stringsAsFactors = FALSE,
    row.names = 1:nrow(global_papers)
  ))
  
  output$data <- DT::renderDataTable({
    DT::datatable(
      df$data, escape = FALSE, selection = 'none',
    )
    }, server = FALSE
  )

  # redirect...  
  observeEvent(input$lastClick, {
    #print(input$lastClickId)
    selectedRow <- as.numeric(strsplit(input$lastClickId, "_")[[1]][2])
    #info about result type...
    vals$type =  "study"
    #pass code of the study...
    vals$text <- as.numeric(global_papers[selectedRow,1])
    print(vals$text)
    callModule(session = parent, module = resultsFunc, id = "id_results",vals)
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
    }
  )
  
}