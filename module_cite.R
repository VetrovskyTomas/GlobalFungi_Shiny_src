# Function for module UI
citeUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "How to cite:"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      sidebarPanel(width = 12,    
        fluidRow(
          column(12, "Morais, D. et al. (2019) Best fungal database ever., Nature & Science 433 (1-2), 401-413")
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