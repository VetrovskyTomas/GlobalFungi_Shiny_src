# Function for module UI

aboutusUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h2(id="welcome_title", "About us"),
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
aboutusFunc <- function(input, output, session) {
}
