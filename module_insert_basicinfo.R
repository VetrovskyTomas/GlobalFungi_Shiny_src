# Function for module UI
insertBasicUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(12, h2(id="section_title",  "Thank you for choosing to submit your data to the GlobalFungi Database! Before submitting your data, please fill in the information below."))
    ),
    br(),
    fluidRow(
      column(6, textInput(inputId = ns("study_contributor"), label = "Name and Surname of the contributor:"))
    ),
    fluidRow(
      column(6, textInput(inputId = ns("study_email1"), label = "e-mail:"))
    ),
    fluidRow(
      column(6, textInput(inputId = ns("study_email2"), label = "confirm your e-mail:"))
    ),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_affiliation_institute"), label = "Primary affiliation - institution:", height = "32px"))
    ),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_affiliation_country"), label = "Primary affiliation - country:", height = "32px"))
    ),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_ORCID"), label = "ORCID: (if available)", height = "32px"))
    ),
    br(),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_title"), label = "Study title:", height = "32px"))
    ),
    fluidRow(
      column(12, textAreaInput(inputId = ns("study_authors"), label = "List of all authors:", placeholder = "e.g.: Vetrovsky, T., Baldrian, P. and Morais, D.", height = "32px"))
    ),
    fluidRow(
      column(2, textInput(inputId = ns("study_year"), label = "Year of publication:"))
    ),
    fluidRow(
      column(5, textInput(inputId = ns("study_journal"), label = "Journal:"))
    ),
    fluidRow(
      column(5, textInput(inputId = ns("study_volume"), label = "Volume:"))
    ),
    fluidRow(
      column(5, textInput(inputId = ns("study_pages"), label = "Pages:"))
    ),
    fluidRow(
      column(5, textInput(inputId = ns("study_doi"), label = "DOI of the paper:"))
    ),
    fluidRow(
      column(12, textInput(inputId = ns("study_repository"), label = "Public repository, where sequencing data are available:"))
    ),
    br(),
    fluidRow(
      column(12, checkboxInput(inputId = ns("study_confirm_1"), label = "I declare that I am the first or the corresponding author of the paper above", FALSE))
    ),
    fluidRow(
      column(12, checkboxInput(inputId = ns("study_confirm_2"), label = "I declare that the sequencing data from my samples are publicly available in an online repository", FALSE))
    ),
    fluidRow(
      column(12, checkboxInput(inputId = ns("study_confirm_3"), label = "I give my consent to the GlobalFungi team to make any sample data / metadata that I submit permanently publicly visible on the database webpage (https://globalfungi.com)", FALSE))
    ),
    fluidRow(
      column(12, checkboxInput(inputId = ns("study_confirm_4"), label = "I give my consent to the GlobalFungi team to contact me by e-mail in case that there are some queries about my sequencing data / metadata", FALSE))
    ),
    fluidRow(
      column(12, selectizeInput(ns("study_include"), 
        label = "I wish to have my name, surname, primary affiliation and country included in the list of collaborators on the database webpage (http://globalfungi.com)", 
        choices = c("YES","NO"), width = "300px", selected = 1, multiple = FALSE))
    ),
    fluidRow(
      column(12, selectizeInput(ns("study_coauthor"), 
        label = "When my data are posted online in one of the new releases of the database, I wish my name to appear at the 'GlobalFungi Group Author' list. For that purpose, I agree to share my e-mail, affiliation and ORCID for the potential submission of papers in publications describing the database content, its development, or metastudies using the whole database should it include the 'GlobalFungi Group Author'", 
        choices = c("YES","NO"), width = "300px", selected = 1, multiple = FALSE))
    ),
    fluidRow(
      column(8, actionButton(ns("buttStart"), label = "Process basic info", icon = icon("microchip")))
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
      } else if (input$study_email1 == "") {
        return("Email field is empty!")
      } else if (input$study_email2 == "") {
        return("Email confirmation field is empty!")
      } else if (input$study_email1 != input$study_email2) {
        return("Email address has a typo!")
      } else if (input$study_affiliation_institute == "") {
        return("Affiliation institute field is empty!")
      } else if (input$study_affiliation_country == "") {
        return("Affiliation country field is empty!")
      } else if (input$study_confirm_1 == FALSE) {
        return("We need your confirmation!")
      } else if (input$study_confirm_2 == FALSE) {
        return("We need your confirmation!")
      } else if (input$study_confirm_3 == FALSE) {
        return("We need your confirmation!")
      } else if (input$study_confirm_4 == FALSE) {
        return("We need your confirmation!")
      }
      return("")
    })
  
  observeEvent(input$buttStart, {
    check_inputs <- study_input()
    if (check_inputs != "") {
      alert(paste0("Missing/incorrect value - ",study_input()))
      study$info <- paste0("Missing value - ",check_inputs,"\n")
    } else {
      print("You processed the basic info...")
      
      time <- format(Sys.time(), "%Y %b %d %X")
      md5 <- as.character(sapply(paste(time, input$study_contributor, input$study_title), digest, algo="md5", serialize=F))
      print(md5)
      
      study$basic$study_email <- input$study_email1
      study$basic$study_hash <- md5
      
      # test duplicate
      query <- sprintf(paste0("SELECT `email` FROM ",options()$mysql$study," WHERE hash = '",md5,"';"))
      result <- data.table(sqlQuery(query))
      print(paste("RESULT FOR UNIQUE RECORDS: ",nrow(result)))
      if (nrow(result)==0) {
        # # write it...
        query <- paste0("INSERT INTO ",options()$mysql$study,
                        " (hash, contributor, email, affiliation_institute, affiliation_country, ORCID, title, authors, year, journal, volume, pages, doi, repository, include, coauthor, email_confirmed, submission_finished, date) VALUES ('",
                        md5, "', '", 
                        safeSqlQueryVal(input$study_contributor), "', '",
                        safeSqlQueryVal(input$study_email1), "', '",
                        safeSqlQueryVal(input$study_affiliation_institute), "', '",
                        safeSqlQueryVal(input$study_affiliation_country), "', '",
                        safeSqlQueryVal(input$study_ORCID), "', '",
                        safeSqlQueryVal(input$study_title), "', '",
                        safeSqlQueryVal(input$study_authors), "', '",
                        safeSqlQueryVal(input$study_year), "', '",
                        safeSqlQueryVal(input$study_journal), "', '",
                        safeSqlQueryVal(input$study_volume), "', '",
                        safeSqlQueryVal(input$study_pages), "', '",
                        safeSqlQueryVal(input$study_doi), "', '",
                        safeSqlQueryVal(input$study_repository), "', '",
                        safeSqlQueryVal(input$study_include), "', '",
                        safeSqlQueryVal(input$study_coauthor), "', 0, 0, '",
                        format(Sys.time(), "%Y %b %d %X"), "')")
        sqlQuery(query)
      } else {
        print(paste("duplicate query for md5: ",md5))
      }
      ###############################################################################
      
    }
  })
}
