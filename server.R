##############
### SERVER ###
##############

server <- function(session, input, output) {
  #hide sidebar collabse button...  
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  #
  output$menu <- renderMenu({
    #
  })
  
  vals <- reactiveValues()
  vals$type <- 'none'
  vals$text <- 'No results yet!'
  #################################################
  main_session <<- session
  #home screen...
  callModule(module = homeFunc, id = "id_home", samples)
  callModule(module = analysisFunc, id = "id_analysis", parent = session)
  callModule(module = searchFunc, id = "id_search", parent = session)
  callModule(module = studiesFunc, id = "id_studies", parent = session)
  callModule(module = insertFunc, id = "id_insert")
  callModule(module = citeFunc, id = "id_cite")
  callModule(module = helpFunc, id = "id_help")
  callModule(module = outputFunc, id = "id_results", vals, parent = session)
  #################################################
  # info about connection...
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
}