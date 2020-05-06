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
          column(12, global_info[,"citation"])
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