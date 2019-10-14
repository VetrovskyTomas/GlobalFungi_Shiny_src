# Function for module UI
helpUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1(id="welcome_title", "Help"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      sidebarPanel(width = 12,    
        fluidRow(
          column(12, "Here is a description of individual functions...")
        )
      ),
      # Main panel for outputs ----
      mainPanel()
    ),
      #leave me a message...
      h1(id="info_title", "Have a question?"),
      sidebarLayout(   
      sidebarPanel(width = 12,
        textInput("from", "From:", value="from@gmail.com"),
        #textInput("to", "To:", value="to@gmail.com"),
        textInput("subject", "Subject:", value=""),
        textAreaInput(ns("textSeq"), "Write you message here:", 
                      "", width="100%", height = "200px"),
        actionButton("send", "Send mail")
      ),
      
      mainPanel(    
        
      )
      )
  )
  
}

# Function for module server logic
helpFunc <- function(input, output, session) {
  #send email...
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    to <- isolate("kostelecke.uzeniny@seznam.cz")
    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    sendmail(from, to, subject, msg)
  })
}