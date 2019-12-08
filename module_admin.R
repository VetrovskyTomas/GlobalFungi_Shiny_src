# Function for module UI
adminUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    h2(id="welcome_title", "Settings"),
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        # inpusts...
        selectInput(ns('selectfile'),'Select File',choice = list.files(global_tables_path)),
        textOutput(ns('fileselected'))
    )
  )
}

# Function for module server logic
adminFunc <- function(input, output, session) {
  
  output$fileselected <- renderText({
    paste0('You have selected: ', input$selectfile)
  })
  
}