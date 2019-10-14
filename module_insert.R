# Function for module UI
insertUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  
  fluidPage(
    # App title ----
    h1(id="welcome_title", "Insert sample metadata"),
    
    tabsetPanel(id = "inTabset",
      ######################################################
      tabPanel(title = "Study info", value = "panel2",
        h1(id="info_title", "Study details"),
        sidebarPanel(
          checkboxInput("Published", "Published", TRUE)
        )
      ),
      tabPanel(title = "Samples info", value = "panel2",
        h1(id="info_title", "Samples details"),
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
          # Sidebar panel for inputs ----
          sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
          ),
          # Main panel for displaying outputs ----
          mainPanel(
            # Output: Data file ----
            tableOutput("contents")
          )
        )
      ),
      tabPanel(title = "Data upload", value = "panel2",
               h1(id="info_title", "Data upload"),
               sidebarPanel(
                 # Input: Select a file ----
                 fileInput("file1", "Choose sequence files",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 checkboxInput("Published", "Published", TRUE)
               )
      )
      ###############################################
    )
  )
}

# Define server logic to read selected file ----
insertFunc <- function(input, output, session) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}
