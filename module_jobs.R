# Function for module UI
jobsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='info.png', height = 56)),
                   column(11, h2(id="header_title", "Jobs"))
                 )
    ),
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
                 fluidRow(
                   column(12, 
                          includeMarkdown("markdown_jobs.txt")
                   )
                 )
    )
  )
}

# Function for module server logic
jobsFunc <- function(input, output, session) {

}