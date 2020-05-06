# Function for module UI
insertIntroUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
      fluidRow(
        column(2, img(src='insert.png')),
        column(6, br(), h2(id="section_title", "Inserting your study guide")),
        column(4, br(), actionButton(ns("buttStart"), label = "Start submission", icon =icon("play-circle")))
      ),
      fluidRow(column(12, includeMarkdown("markdown_insert.txt")))
    )
  )
}

# server logic to read selected file ----
insertIntroFunc <- function(input, output, session, study) {
  
  ns <- session$ns
  
  observeEvent(input$buttStart, {
    print("You started new submission...")
    study$basic <- NULL
    study$info <- "You started new submission..."
    study$start <- TRUE
  })
}
