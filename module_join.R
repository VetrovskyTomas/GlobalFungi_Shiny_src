# Function for module UI
joinUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;", img(src='join.png', height = 56)),
                   column(11, h2(id="header_title", "Join mailing list"))
                 )
    ),
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
                 fluidRow(
                   column(4,h2(id="section_title", "Add me to mailing list"))
                 ),
                 fluidRow(
                   column(3, textInput(ns("name"), "Name:", value="")),
                 ),
                 fluidRow(
                   column(3,  textInput(ns("email"), "Email:", value=""))
                 ),
                 fluidRow(
                   column(2,  actionButton(ns("submit"), "Submit", icon = icon("address-book")))
                 )
    )
  )
}

# Function for module server logic
joinFunc <- function(input, output, session) {
  # check email raw...
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
  # add to database...
  observeEvent(input$submit, {
    sender <- sender_check(isolate(input$email))
    if (!is.null(sender)){
      name <- isolate(input$name)
      if (name !=""){
        # write it...
        query <- paste0("INSERT INTO ",options()$mysql$maillist, 
                        " (name, email) VALUES ('", 
                        safeSqlQueryVal(name), "', '", 
                        safeSqlQueryVal(sender), "')")
        sqlQuery(query)
        # reset fields..
        reset("name")
        reset("email")
        # get info...
        alert("Your email address was inserted to mailing list.")
      } else {
        alert("Please specify your name!")
      }
    } else {
      alert("Not valid email address.")
    }
  }, ignoreInit = TRUE)
}