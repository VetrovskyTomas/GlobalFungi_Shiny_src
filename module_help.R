# Function for module UI
helpUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    h2(id="welcome_title", "Have a question?"),
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
        # inpusts...
        textInput(ns("from"), "From:", value="from@gmail.com"),
        textInput(ns("subject"), "Subject:", value=""),
        textAreaInput(ns("message"), "Write you message here:","", width="100%", height = "200px"),
        actionButton(ns("send"), "Send mail")
    )
  )
}

# Function for module server logic
helpFunc <- function(input, output, session) {
  #send email...
  observeEvent(input$send, {
    from <- isolate(input$from)
    to <- isolate("kostelecke.uzeniny@seznam.cz")
    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    #print(paste("try to send...",from, to, subject, msg))
    sendmail(from, to, subject, msg)
    alert("The mail has been sent successfully.")
  }, ignoreInit = TRUE)
  
}