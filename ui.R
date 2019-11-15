######################
### USER INTERFACE ###
######################
itemIds <- c(team = "#team", team2 = "#team2", login = "#login")
ui <- tagList(
  dashboardPage(
    dashboardHeader(title = "Fungal Metastudy Database v 0.11", titleWidth = 240,
      # dropdown...
      tags$li(class = "dropdown",
              tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: gray;"),
              tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
    ),
    # sidebar...
    dashboardSidebar(
      width = 200,
      #disable = FALSE, width = NULL, 
      collapsed = FALSE,
      sidebarMenuOutput("menu")
    ),
    # body...
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
                resultsUI(id = "id_results")
        ),
        tabItem("fmd_aboutus",
                aboutusUI(id = "id_aboutus")
        ),
        tabItem("fmd_analysis_group",
                analysisGroupUI(id = "id_analysis_group")
        )
      )
    )
  )
)