# Function for module UI
aboutusUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    h2(id="welcome_title", "Have a question? Leave a message..."),
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
aboutusFunc <- function(input, output, session) {
  
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
        tab <- data.frame(email = sender, subject = isolate(input$subject), message = message, stringsAsFactors = F)
        write.table(t(tab), file = paste0(global_messages_path, sender, "_", as.integer(Sys.time()),".msg"), quote = F, col.names = F, row.names = F)
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
