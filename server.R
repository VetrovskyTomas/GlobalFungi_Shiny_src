library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table) # fast read
library(leaflet) # interactive world map
library(leaflet.extras)
library(sp)
library(DT)
library(ECharts2Shiny)
library(digest)
library(stringr)
library(stringi)
library(readxl)
library(geoshaper)

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
    ifelse(input$isMobile, "mobile", "PC")
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
  
  # usesrs login...
  observeEvent(input$login, {
    if(logged_in()) {
      logged_in(FALSE)
    } else {
      showModal(dataModal())
    }
  })
  
  # show "Login" or "Logout" depending on whether logged out or in
  output$logintext <- renderText({
    if(logged_in()) return("Logout here.")
    return("Login here")
  })
  
  # show text of logged in user
  output$logged_user <- renderText({
    if(logged_in()) return("User is logged in.")
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
    len <- length(users$IPs)
    IP <- session$request[["REMOTE_ADDR"]]
    print(IP)
    users$IPs <- c( users$IPs , IP)
    users$IPs <- unique(users$IPs)
    
    users$count = users$count + 1
    if (users$count > users$max){
      users$max = users$max + 1
    }
    
    # if (length(users$IPs)>len){
    #   write.table(users$IPs,file = "IP_addresses.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
    # }
  })
  
  # someone ended session...
  onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
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
    # species redirection by link...
    if (!is.null(query[['species']])) {
      vals$type <- 'species'
      vals$text <- query[['species']]
      print(paste0("The url query species value is ", vals$text))
      callModule(module = resultsFunc, id = "id_results", vals)
      updateTabItems(session, "menu_tabs", "fmd_results")
    }
  })
  
  #
  output$menu <- renderMenu({
    sidebarMenu(id = "menu_tabs",
                menuItem("Home", icon = icon("home"), tabName = "fmd_home"),
                menuItem("Sequence Analysis", icon = icon("dna"), tabName = "fmd_analysis"),
                menuItem("Cluster analysis", icon = icon("indent"), tabName = "fmd_analysis_group"),
                menuItem("Search", icon = icon("search"), tabName = "fmd_search"),
                menuItem("Studies", icon = icon("microscope"), tabName = "fmd_studies"),
                menuItem("Geosearch", icon = icon("globe"), tabName = "fmd_geoshape"),
                menuItem("How to cite", icon = icon("smile-wink"), tabName = "fmd_cite"),
                menuItem("Help", icon = icon("question-circle"), tabName = "fmd_help"),
                ###########################
                hidden(tags$div(
                  id = "hidden",
                  sidebarMenu(
                  tags$hr(),
                  menuItem("Settings", icon = icon("user-cog"), tabName = "fmd_admin",
                           badgeLabel = "admin", badgeColor = "orange")
                  )
                )),
                ###########################
                tags$hr(),
                #result page...
                menuItem("Results", icon = icon("poll"), tabName = "fmd_results", selected = !is.null(query[['SH']])),
                tags$hr(),
                menuItem("Insert your study", icon = icon("file-upload"), tabName = "fmd_insert",
                         badgeLabel = "in progress", badgeColor = "red"),
                tags$hr(),
                # about the database
                menuItem("Leave a message", icon = icon("info-circle"), tabName = "fmd_aboutus"),
                tags$hr(),
                # url info...
                fluidPage(
                  verbatimTextOutput("urlText")
                ),
                fluidPage(
                  verbatimTextOutput("copyright")
                )
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
  callModule(module = insertFunc, id = "id_insert")
  callModule(module = citeFunc, id = "id_cite")
  callModule(module = helpFunc, id = "id_help")
  callModule(module = resultsFunc, id = "id_results", vals)
  callModule(module = aboutusFunc, id = "id_aboutus")
  callModule(module = analysisGroupFunc, id = "id_analysis_group", parent = session)
  callModule(module = adminFunc, id = "id_admin")
  #################################################
  
  # info about connection...
  output$urlText <- renderText({
    paste(sep = "",
          "# sessions:", users$count, "\n",
          "# sessions max:", users$max, "\n",
          "# unique users:", length(users$IPs), "\n",
          "protocol: ", session$clientData$url_protocol, "\n",
          "host: ", session$clientData$url_hostname, "\n",
          "path: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  
  # copyright...
  output$copyright <- renderText({
    paste(sep = "",
          "    site design    \n",
          "         &         \n",
          "    programming    \n",
          "  Tomas Vetrovsky  \n",
          "     (c) 2019      \n")
  })
  
}