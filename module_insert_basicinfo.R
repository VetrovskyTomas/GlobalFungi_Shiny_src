# Function for module UI
insertBasicUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(h1(id="welcome_title", "Basic info (Please fill all the fields)"),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_title"), label = "Study title", height = "32px"))
    ),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_authors"), label = "All authors", placeholder = "e.g.: Vetrovsky, T., Baldrian, P. and Morais, D.", height = "32px"))
    ),
    fluidRow(
      column(2, textInput(inputId = ns("study_year"), label = "Year of publication")),
      column(5, textInput(inputId = ns("study_journal"), label = "Journal")),
      column(5, textInput(inputId = ns("study_doi"), label = "DOI of the paper"))
    ),
    fluidRow(
      column(6, textInput(inputId = ns("study_contributor"), label = "Contributor Name")),
      column(6, textInput(inputId = ns("study_email"), label = "Contributor E-mail"))
    ),  
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_affiliation"), label = "Contributor Affiliation", height = "32px"))
    ),
    fluidRow(
      column(8, checkboxInput(inputId = ns("study_confirm"), label = "Here I am confirming that I am first or corresponding author of the study.", FALSE)),
      column(4, actionButton(ns("buttStart"), label = "Process basic info", icon = icon("microchip")))
    )
    )
  )
}

# server logic to read selected file ----
insertBasicFunc <- function(input, output, session, study) {

  ns <- session$ns  
  
    # check if inputs are correct...
    study_input <- reactive({
      if (input$study_title == "") {
        return("Title field is empty!")
      } else if (input$study_authors == "") {
        return("Authors field is empty!")
      } else if (input$study_year == "") {
        return("Year field is empty!")
      } else if (input$study_journal == "") {
        return("Journal field is empty!")
      } else if (input$study_doi == "") {
        return("DOI field is empty!")
      } else if (input$study_contributor == "") {
        return("Contributor field is empty!")
      } else if (input$study_email == "") {
        return("Email field is empty!")
      } else if (input$study_affiliation == "") {
        return("Affiliation field is empty!")
      }
      return("")
    })
  
  observeEvent(input$buttStart, {
    check_inputs <- study_input()
    if (check_inputs != "") {
      alert(paste0("Missing value - ",study_input()))
      study$info <- paste0("Missing value - ",check_inputs,"\n")
    } else {
      print("You processed the basic info...")
      study$info <- "You processed the basic info..."
      study$basic$test <- "OK"
    }
  })
}
