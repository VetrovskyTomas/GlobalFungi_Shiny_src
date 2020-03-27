# Function for module UI
aboutusUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='aboutus.png', height = 56)),
        column(11, h2(id="header_title", paste0("About ",global_info[,"name"])))
      )
    ),
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
      fluidRow(
        column(12, img(src='aboutus.png', height = 128), h2(id="section_title", "About GlobalFungi"))
      ),
      fluidRow(
        column(12, 
          # this will be changed to the data folder.
          includeMarkdown("markdown_aboutus.txt")
        )
      )
    )
  )
}

# Function for module server logic
aboutusFunc <- function(input, output, session) {
  #
}