######################
### USER INTERFACE ###
######################
ui <- tagList(
  dashboardPage(
    dashboardHeader(
      title = "Fungal Metastudy Database v 0.1",
      titleWidth = 380,
      #announcements...
      dropdownMenu(type = "notifications",
                   notificationItem(
                     text = "5 new users today",
                     icon("users")
                   ),
                   notificationItem(
                     text = "12 items delivered",
                     icon("truck"),
                     status = "success"
                   ),
                   notificationItem(
                     text = "Server load at 86%",
                     icon = icon("exclamation-triangle"),
                     status = "warning"
                   )
      ),
      #progress...
      dropdownMenu(type = "tasks", badgeStatus = "success",
                   taskItem(value = 90, color = "green",
                            "Documentation"
                   ),
                   taskItem(value = 17, color = "aqua",
                            "Project X"
                   ),
                   taskItem(value = 75, color = "yellow",
                            "Server deployment"
                   ),
                   taskItem(value = 80, color = "red",
                            "Overall project"
                   )
      )
    ),
    #sidebar...
    dashboardSidebar(
      width = 200,
      #disable = FALSE, width = NULL, 
      collapsed = FALSE,
      sidebarMenu(id = "menu_tabs",
                  menuItem("Home", icon = icon("home"), tabName = "fmd_home"),
                  menuItem("Sequence Analysis", icon = icon("dna"), tabName = "fmd_analysis"),
                  menuItem("Search", icon = icon("search"), tabName = "fmd_search"),
                  menuItem("Studies", icon = icon("microscope"), tabName = "fmd_studies"),
                  menuItem("How to cite", icon = icon("smile-wink"), tabName = "fmd_cite"),
                  menuItem("Help", icon = icon("info-circle"), tabName = "fmd_help"),
                  tags$hr(),
                  #result page...
                  menuItem("Results", icon = icon("poll"), tabName = "fmd_results"),
                  tags$hr(),
                  menuItem("Insert your study", icon = icon("file-upload"), tabName = "fmd_insert",
                           badgeLabel = "in progress", badgeColor = "red"),
                  tags$hr(),
                  # url info...
                  fluidPage(
                    verbatimTextOutput("urlText")
                  )
      ),
      sidebarMenuOutput("menu")
    ),
    #body...
    dashboardBody(
      useShinyjs(),
      # link the page style...
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      # fix the header...
      tags$script(HTML("$('body').addClass('fixed');")),
      # body code...
      # buttons...
      tabItems(
        #home
        tabItem("fmd_home",
                homeUI(id = "id_home")
        ),
        #sequence analysis
        tabItem("fmd_analysis",
                analysisUI(id = "id_analysis")
        ),
        #sequence analysis
        tabItem("fmd_search",
                searchUI(id = "id_search")
        ),
        #sequence analysis
        tabItem("fmd_studies",
                studiesUI(id = "id_studies")
        ),
        #cite
        tabItem("fmd_cite",
                citeUI(id = "id_cite")
        ),
        #help
        tabItem("fmd_help",
                helpUI(id = "id_help")
        ),
        #insert study
        tabItem("fmd_insert",
                insertUI(id = "id_insert")
        ),
        #insert study
        tabItem("fmd_results",
                outputUI(id = "id_results")
        )
      )
      #end dashboardPage
      #,tags$footer("Created by Tomas Vetrovsky & Daniel Morais (c) 2019", align = "center")
    )
  )
)