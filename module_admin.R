# Function for module UI
adminUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='settings.png', height = 56)),
                   column(11, h2(id="header_title", "Welcome admin"))
                 )
    ),
    # table selector
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        # inputs...
        selectInput(ns('select_tab'),'Select table',choice = c("messages")),
        actionButton(ns("refresh_tab"), "Refresh", icon = icon("redo")),
        DT::dataTableOutput(ns("table_selected"))
    )
  )
}

# Function for module server logic
adminFunc <- function(input, output, session) {
  
  observeEvent(input$refresh_tab, {
    observe(
      output$table_selected <- DT::renderDataTable({
        query <- sprintf(paste0("SELECT * FROM ",input$select_tab," ORDER BY id DESC"))
        table <- data.table(sqlQuery(query))
      })
    )
  })
  
}