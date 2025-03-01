# Function for module UI
collaboratorsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='collaborators.png', height = 56)),
                   column(11, h2(id="header_title", "Collaborators"))
                 )
    ),
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
                 fluidRow(
                   column(12, 
                          # this will be changed to the data folder.
                          includeMarkdown("markdown_collaborators.txt")
                   )
                 )
    )
  )
}

# Function for module server logic
collaboratorsFunc <- function(input, output, session) {
  #
}