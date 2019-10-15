library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table) # fast read
library(leaflet) # interactive world map
library(DT)
library(ECharts2Shiny)

##############
### SERVER ###
##############

server <- function(session, input, output) {
  
  # someone started session...
  onSessionStart = isolate({
    users$count = users$count + 1
    if (users$count > users$max){
      users$max = users$max + 1
    }
  })
  
  # someone ended session...
  onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
    })
  })  
  
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
          "# users:", users$count, "\n",
          "max users:", users$max, "\n",
          "protocol: ", session$clientData$url_protocol, "\n",
          "host: ", session$clientData$url_hostname, "\n",
          "path: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
}