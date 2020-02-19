######################
### USER INTERFACE ###
######################
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

itemIds <- c(team = "#team", team2 = "#team2", login = "#login")

ui <- tagList(
  dashboardPage(
    dashboardHeader(title = paste(global_info[,"name"],global_info[,"version"]), titleWidth = 200,
      # dropdown...
      tags$li(class = "dropdown",
        tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: gray;"),
        tags$li(class = "dropdown", fluidPage(mobileDetect('isMobile'), textOutput('isItMobile')), style = "padding-top: 15px; padding-bottom: 15px; color: gray;"),
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
      tags$head(includeScript("google-analytics.js")),
      tags$head(tags$link(rel="shortcut icon", href="geosearch.png")),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      # fix the header...
      tags$script(HTML("$('body').addClass('fixed');")),
      # body code...
      # buttons...
      tabItems(
        # home
        tabItem("fmd_home",
                homeUI(id = "id_home")
        ),
        #search taxon
        tabItem("fmd_search",
                searchUI(id = "id_search")
        ),
        #sequence analysis
        tabItem("fmd_analysis",
                analysisUI(id = "id_analysis")
        ),
        #studies
        tabItem("fmd_studies",
                studiesUI(id = "id_studies")
        ),
        #geoshape
        tabItem("fmd_geoshape",
                geoshapeUI(id = "id_geoshape")
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
        # about us
        tabItem("fmd_aboutus",
                aboutusUI(id = "id_aboutus")
        ),
        # settings
        tabItem("fmd_admin",
                adminUI(id = "id_admin")
        ),
        # settings
        tabItem("fmd_message",
                messageUI(id = "id_message")
        ),
        # settings
        tabItem("fmd_collaborators",
                collaboratorsUI(id = "id_collaborators")
        )
      )
    )
  )
)