# Function for module UI
helpUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='help.png', height = 56)),
                   column(11, h2(id="header_title", "About us"))
                 )
    ),
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
                 fluidRow(
                   column(12, 
                          # this will be changed to the data folder.
                          if(!file.exists("markdown_file.txt")){
                            download.file(url = "https://drive.google.com/uc?export=download&id=1XQ_0klmuXAK2YkdUivnWFKO4KM6WqUWk", 
                                          destfile = "markdown_file.txt")
                            includeMarkdown("markdown_file.txt")
                          } else {
                            includeMarkdown("markdown_file.txt")
                          }
                   )
                 )
    )
  )
}

# Function for module server logic
helpFunc <- function(input, output, session) {

}