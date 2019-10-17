# Function for module UI

aboutusUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    #h1(id="welcome_title", "The Fungal Metastudy Consortium Database"),
    #titlePanel("The Fungal Metastudy Consortium Database"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      sidebarPanel(width = 12,    
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
      ),
      # Main panel for outputs ----
      mainPanel(
        
      )
    )
  )
  
}

# Function for module server logic
aboutusFunc <- function(input, output, session) {
}
