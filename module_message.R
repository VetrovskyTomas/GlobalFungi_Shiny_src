# Function for module UI
messageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='message.png', height = 56)),
                   column(11, h2(id="header_title", "Have a question?"))
                 )
    ),
    # content
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
                 # inpusts...
                 textInput(ns("from"), "From:", value="from@gmail.com"),
                 textInput(ns("subject"), "Subject:", value=""),
                 textAreaInput(ns("message"), "Write your message here:","", width="100%", height = "200px"),
                 actionButton(ns("send"), "Send mail")
    )
  )
}

# Function for module server logic
messageFunc <- function(input, output, session) {
  
  sender_check <- function(sender){
    if (is.null(sender)) {
      return(NULL)
    } else if (sender == 'from@gmail.com') {
      return(NULL)
    } else if (grepl(" ", sender)) {
      return(NULL)
    } else if (grepl("@", sender)) {
      return(sender)
    }
  }
  #send email...
  observeEvent(input$send, {
    sender <- sender_check(isolate(input$from))
    if (!is.null(sender)){
      message <- isolate(input$message)
      if (message !=""){
        # write it...
        query <- paste0("INSERT INTO ",options()$mysql$messages, 
                                " (email, subject, message) VALUES ('", 
                                safeSqlQueryVal(sender), "', '", safeSqlQueryVal(isolate(input$subject)), "', '", 
                                safeSqlQueryVal(message), "')")
        sqlQuery(query)
        
        # send email to global fungi...
        sendEmail(safeSqlQueryVal(sender), "info@globalfungi.com", safeSqlQueryVal(isolate(input$subject)), safeSqlQueryVal(message))

        # reset fields..
        reset("from")
        reset("subject")
        reset("message")
        # get info...
        alert("The message was processed successfully.")
      } else {
        alert("There is no message.")
      }
    } else {
      alert("Not valid email address.")
    }
  }, ignoreInit = TRUE)
  
}
