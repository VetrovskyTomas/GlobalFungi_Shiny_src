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
  
  # set default vals...
  vals <- reactiveValues()
  vals$type <- 'none'
  vals$text <- 'No results yet!'
  # TRY TO PROCESS URL QUERY
  # e.g.: 127.0.0.1:5048/?SH=SH000160
  query <- NULL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['SH']])) {
      vals$type <- 'SH'
      vals$text <- query[['SH']]
      print(paste0("The url query SH value is ", vals$text))
      updateTabItems(session, "menu_tabs", "fmd_results")
    }
  })
  
  #
  output$menu <- renderMenu({
    sidebarMenu(id = "menu_tabs",
                menuItem("Home", icon = icon("home"), tabName = "fmd_home"),
                menuItem("Sequence Analysis", icon = icon("dna"), tabName = "fmd_analysis"),
                menuItem("Search", icon = icon("search"), tabName = "fmd_search"),
                menuItem("Studies", icon = icon("microscope"), tabName = "fmd_studies"),
                menuItem("How to cite", icon = icon("smile-wink"), tabName = "fmd_cite"),
                menuItem("Help", icon = icon("info-circle"), tabName = "fmd_help"),
                tags$hr(),
                #result page...
                menuItem("Results", icon = icon("poll"), tabName = "fmd_results", selected = !is.null(query[['SH']])),
                tags$hr(),
                menuItem("Insert your study", icon = icon("file-upload"), tabName = "fmd_insert",
                         badgeLabel = "in progress", badgeColor = "red"),
                tags$hr(),
                # about the database
                menuItem("About us", icon = icon("info-circle"), tabName = "fmd_aboutus"),
                tags$hr(),
                # url info...
                fluidPage(
                  verbatimTextOutput("urlText")
                )
    )
  })
  
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
  callModule(module = aboutusFunc, id = "id_aboutus")
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