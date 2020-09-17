# Function for module UI
citeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='cite.png', height = 56)),
                   column(11, h2(id="header_title", "How to cite"))
                 )
    ),
    # content
    sidebarLayout(
      sidebarPanel(width = 12,    
        fluidRow(
          column(12, 
            includeMarkdown("markdown_citation.txt")
          )),
          fluidRow(
          column(12,
                 tags$a(href="https://www.nature.com/articles/s41597-020-0567-7", "DOI: https://doi.org/10.1038/s41597-020-0567-7", target="_blank")
          )
      )
      ),
      # Main panel for outputs ----
      mainPanel()
    )
  )
  
}

# Function for module server logic
citeFunc <- function(input, output, session) {
  #
}