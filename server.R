##############
### SERVER ###
##############
main_username <- "admin"
main_password <- "Leho"
my_username <- "test"
my_password <- "test"

server <- function(session, input, output) {
  # test mobile device
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "mobile", "NOTE: optimal usability would be achieved using Firefox or Chrome on PC")
  })
  
  # UI for user login modal dialog
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # UI for a main modal dialog
  dataModalMain <- function(failed = FALSE) {
    modalDialog(
      textInput("username_main", "Username:"),
      passwordInput("password_main", "Password:"),
      footer = tagList(
        actionButton("ok_main", "OK")
      )
    )
  }  
    
  logged_in <- reactiveVal(FALSE)

  ##############################################################
  # MAIN LOCK - THIS WILL BE REMOVED LATER
  # observe({
  #   showModal(dataModalMain())
  # })
  
  observe({
    req(input$ok_main)
    isolate({
      Username <- input$username_main
      Password <- input$password_main
    })
    Id.username <- which(main_username == Username)
    Id.password <- which(main_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        print(paste0("You are logged in..."))
        removeModal()
      } else {
        print(paste0("Wrong authentication..."))
      }     
    }
  })
  ##############################################################
  
  # show "Login" or "Logout" depending on whether logged out or in
  shinyjs::onclick("login_img",  if(logged_in()) {
    logged_in(FALSE)
  } else {
    showModal(dataModal())
  })
  
  # show text of logged in user
  output$logged_user <- renderText({
    if(logged_in()) return("Admin is logged in :)")
    return("")
  })
  
  observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        logged_in(TRUE)
        print(paste0("You are logged in..."))
        removeModal()
      } else {
        print(paste0("Wrong authentication..."))
      }     
    }
  })
  

  # someone started session...
  onSessionStart = isolate({
    global_session <<- global_session + 1
  })
  
  # someone ended session...
  onSessionEnded(function() {
    isolate({
      # users$count = users$count - 1
    })
  })  
  
  #hide sidebar collabse button...
  observe({
    if (!input$isMobile) {
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
    }
  })
  # set default vals...
  vals <- reactiveValues()
  vals$type <- 'none'
  vals$text <- 'No results yet!'
  # TRY TO PROCESS URL QUERY
  # e.g.: 127.0.0.1:5048/?SH=SH000160
  query <- NULL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query)
    # SH redirection by link...
    if (!is.null(query[['SH']])) {
      vals$type <- 'SH'
      vals$text <- query[['SH']]
      print(paste0("The url query SH value is ", vals$text))
      callModule(module = resultsFunc, id = "id_results", vals)
      updateTabItems(session, "menu_tabs", "fmd_results")
    } else 
    # paper redirection by link...
    if (!is.null(query[['paper']])) {
      vals$type <- 'study'
      vals$text <- query[['paper']]
      print(paste0("The url query paper value is ", vals$text))
      callModule(module = resultsFunc, id = "id_results", vals)
      updateTabItems(session, "menu_tabs", "fmd_results")
    } else   
    # species redirection by link...
    if (!is.null(query[['species']])) {
      vals$type <- 'species'
      vals$text <- query[['species']]
      print(paste0("The url query species value is ", vals$text))
      callModule(module = resultsFunc, id = "id_results", vals)
      updateTabItems(session, "menu_tabs", "fmd_results")
    } else   
      # study submission redirection by link...
      if (!is.null(query[['study']])) {
        vals$key <- query[['study']]
        print(paste0("The url query study value is ", vals$text))
        callModule(module = insertFunc, id = "id_insert", vals)
        updateTabItems(session, "menu_tabs", "fmd_insert")
      }
  })
  
  #
  output$menu <- renderMenu({
    sidebarMenu(id = "menu_tabs",
                tags$style(HTML("hr {margin-top: 0.5em; margin-bottom: 0.5em;}")),
                menuItem("Home", icon = icon("home"), tabName = "fmd_home"),
                menuItem("Taxon search", icon = icon("search"), tabName = "fmd_search"),
                menuItem("Sequence search", icon = icon("dna"), tabName = "fmd_analysis"),
                menuItem("Geosearch", icon = icon("globe"), tabName = "fmd_geoshape"),
                menuItem("Studies", icon = icon("microscope"), tabName = "fmd_studies"),
                tags$hr(),
                #result page...
                menuItem("Results", icon = icon("poll"), tabName = "fmd_results", selected = !is.null(query[['SH']])),
                tags$hr(),
                menuItem("How to cite", icon = icon("smile-wink"), tabName = "fmd_cite"),
                menuItem(paste0("About ",global_info[,"name"]), icon = icon("globe-americas"), tabName = "fmd_aboutus"),
                menuItem("Help", icon = icon("question-circle"), tabName = "fmd_help"),
                ###########################
                hidden(tags$div(
                  id = "hidden",
                  sidebarMenu(
                  tags$hr(),
                  menuItem("Settings", icon = icon("user-cog"), tabName = "fmd_admin",
                           badgeLabel = "admin", badgeColor = "orange")
                ))
                ),
                ###########################
                tags$hr(),
                menuItem("Leave a message", icon = icon("info-circle"), tabName = "fmd_message"),
                menuItem("Submit your study", icon = icon("file-upload"), tabName = "fmd_insert", badgeColor = "red"),
                tags$hr(),
                menuItem("Collaborators", icon = icon("people-carry"), tabName = "fmd_collaborators"),
                tags$hr(),
                # copyright...
                fluidPage(
                  tags$img(
                    src = "nick.png",
                    style = 'position: absolute'
                  ),
                  tags$style(type='text/css', '#copyright {background-color: rgb(30,40,44); color: rgb(184,199,206);}'), 
                  verbatimTextOutput("copyright")
                ),
                tags$hr(),
                uiOutput('dynamic_content')
    )
  })
  
  # show/hide special items...
  observe({
    if (logged_in()){
      shinyjs::show("hidden")
    } else {
      shinyjs::hide("hidden")
    }
  })
  #################################################
  main_session <<- session
  #home screen...
  callModule(module = homeFunc, id = "id_home", samples)
  callModule(module = analysisFunc, id = "id_analysis", parent = session)
  callModule(module = searchFunc, id = "id_search", parent = session)
  callModule(module = studiesFunc, id = "id_studies", parent = session)
  callModule(module = geoshapeFunc, id = "id_geoshape")
  callModule(module = insertFunc, id = "id_insert", vals)
  callModule(module = citeFunc, id = "id_cite")
  callModule(module = helpFunc, id = "id_help")
  callModule(module = resultsFunc, id = "id_results", vals)
  callModule(module = messageFunc, id = "id_message")
  callModule(module = aboutusFunc, id = "id_aboutus")
  callModule(module = adminFunc, id = "id_admin")
  callModule(module = collaboratorsFunc, id = "id_collaborators")
  #################################################
  
  # copyright...
  output$copyright <- renderText({
    paste(sep = "",
          "       site design  \n",
          "            &       \n",
          "       programming  \n",
          "     Tomas Vetrovsky\n",
          "      Daniel Morais \n",
          "        (c) 2020    \n")
  })
  
  delay(500,
  output$dynamic_content <- renderUI({
    fluidPage(
      actionButton("elixir_butt", label = NULL, 
                   style = "width: 144px; height: 64px; background: url('elixir_button.png');  background-size: cover; background-position: center;",
                   onclick = paste0("window.open('https://www.elixir-czech.cz/')")
      )
    )
  })
  )
  
}