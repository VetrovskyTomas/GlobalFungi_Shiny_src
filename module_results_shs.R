# Function for module UI with SH
resutsSHsUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(width = "100%", style = "background-color:white;",  
    wellPanel(style = "background-color:white;",  
    fluidRow(
      column(6,downloadButton(ns("downloadSHs"), "Download SH list"))
    ),
    fluidRow(
      br(),
      DT::dataTableOutput(ns("SH_list"))  
    )
    )
  )
}

# Function for module server logic
resutsSHsFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  # show SH list with URL to SH...
  output$SH_list <- DT::renderDataTable(
    DT::datatable({
      data <- variable$SHs[,c("SH","Kingdom","Phylum","Class","Order","Family","Genus","Species")] 
      data <- data %>% mutate(SH = paste0("<a href='", "/?SH=",SH,"' target='_blank'>", SH,"</a>"))
      data <- data %>% mutate(Species = ifelse(!grepl(" sp.", Species), paste0("<a href='", "/?species=",Species,"' target='_blank'>", Species,"</a>"),Species))
      data
    },
    escape = FALSE, selection = 'none')
  )
  # Downloadable csv of selected SHs...
  output$downloadSHs <- downloadHandler(
    filename = "sh_list.txt",
    content = function(file) {
      write.table(variable$SHs, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
}