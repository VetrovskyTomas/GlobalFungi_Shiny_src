# Function for module UI
searchUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    loadEChartsTheme('shine'),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='search_tax.png', height = 56)),
                   column(11, h2(id="header_title", "Search by taxonomy!"))
                 )
    ),
    # search page...
    sidebarPanel(width = "100%", style = "background-color:white;",
      tabsetPanel(id = ns("navbar"),
        tabPanel("SH",
          br(),
          textInput(inputId = ns("search_key_sh"),
            placeholder = "Enter SH, e.g: SH1509013.08FU",
            label = "Select SH:", 
            width = "300px"
          )
        ),
        tabPanel("Species", 
          br(),
          selectizeInput(inputId = ns("search_key_species"),
                         options = list(
                           maxOptions=length(global_species_list),
                           placeholder = 'Please select an option below'
                         ),
                         label = "Select species:", choices = global_species_list, width = "300px",
                         selected = 1, 
                         multiple = FALSE # allow for multiple inputs
          )
        ),
        tabPanel("Genus",
          br(),  
          selectizeInput(inputId = ns("search_key_genus"),
            options = list(
              maxOptions=length(global_genus_list),
              placeholder = 'Please select an option below'
            ),
            label = "Select genus:", choices = global_genus_list, width = "300px",
            selected = 1, 
            multiple = FALSE # allow for multiple inputs
          )
        )
        ),
      fluidRow(
        column(6, actionButton(ns("buttSearch"), label = "Search"))
      )
    ), 
        
        
    # breakdown of search options
    sidebarPanel(width = "100%", style = "background-color:white;",
        fluidRow(
          column(6, h2("Beakdown of search options"))
        ),
        fluidRow(
          column(6, 
            tags$div(id = "class_bar", style="width: 100%;height:300px;"),  # Specify the div for the chart. Can also be considered as a space holder
            deliverChart(div_id = ns("class_bar"))  # Deliver the plotting
          )
        )
      )
    )
}

# Function for module server logic
searchFunc <- function(input, output, session, parent) {
  
  # Submit button...
  observeEvent(input$buttSearch, {
    # to be shared...
    vals <- reactiveValues()
    vals$type <- 'none'
    vals$text <- 'No results yet!'
    
    # get selected table...
    nav_type <- req(isolate(input$navbar))
    #SH...
    if (nav_type == "SH") {
      vals$type <- 'SH'
      vals$text <- input$search_key_sh
    } else 
    # species...
    if (nav_type == "Species") {
      vals$type <- 'species'
      vals$text <- input$search_key_species
    } else 
    # genus...
    if (nav_type == "Genus") {
      vals$type <- 'genus'
      vals$text <- input$search_key_genus
    }
    
    # message...
    print(paste("You are searching for", vals$type, "- value is",vals$text))

    # call results...
    callModule(session = parent, module = resultsFunc, id = "id_results",isolate(vals)) 
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  observe({
      # load species options...
      updateSelectizeInput(session, "search_key_species",
                           choices = global_species_list, 
                           server = TRUE)
      
      # load genus options...
      updateSelectizeInput(session, "search_key_genus", 
                           choices = global_genus_list, 
                           server = TRUE)
  })
  
  observe({
    # make bar chart...
    bar_data <- data.frame(
      name = c("SH","Species","Genus"),
      value = c(length(global_SH_list),length(global_species_list),length(global_genus_list))
    )
    rownames(bar_data) <- bar_data[,1]
    bar_data = subset(bar_data, select = -name )
    renderBarChart(div_id = "class_bar", 
                   data = bar_data, 
                   direction = "vertical", 
                   theme = "shine",
                   show.legend = FALSE)
  })
  
}