# Function for module UI
searchUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    loadEChartsTheme('shine'),
    # search page...
    h1(id="welcome_title", "Search in the database!"),
    sidebarPanel(width = "100%", style = "background-color:white;",
      tabsetPanel(
        tabPanel("SH",
          br(),
          selectizeInput(inputId = ns("search_key_sh"),
                         options = list(
                           maxOptions=length(global_SH_list),
                           placeholder = 'Please select an option below'
                         ),
                         label = "Select SH:", choices = global_SH_list, width = "200px",
                         selected = 1, 
                         multiple = FALSE # allow for multiple inputs
          ),
          fluidRow(
            column(6, actionButton(ns("buttSearch_sh"), label = "Search"))
          )
        ),
        tabPanel("Species", 
          br(),
          selectizeInput(inputId = ns("search_key_species"),
                         options = list(
                           maxOptions=length(global_species_list),
                           placeholder = 'Please select an option below'
                         ),
                         label = "Select species:", choices = global_species_list, width = "400px",
                         selected = 1, 
                         multiple = FALSE # allow for multiple inputs
          ),
          fluidRow(
            column(6, actionButton(ns("buttSearch_species"), label = "Search"))
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
          ),
            fluidRow(
              column(6, actionButton(ns("buttSearch_genus"), label = "Search"))
            )
        )
        )
    ), 
        
        
      # Main panel for outputs ----
      mainPanel(
        fluidRow(
          tags$div(id = "class_bar", style="width: 80%;height:300px;"),  # Specify the div for the chart. Can also be considered as a space holder
          deliverChart(div_id = ns("class_bar"))  # Deliver the plotting
        )
      )
    )
}

# Function for module server logic
searchFunc <- function(input, output, session, parent) {
  # to be shared...
  vals <- reactiveValues()
  
  # Submit button for SH...
  observeEvent(input$buttSearch_sh, {
    #info about result type...
    vals$type =  "SH"
    #info for output page...
    vals$text <- input$search_key_sh
    print("You clicked on search SH....")
    callModule(session = parent, module = resultsFunc, id = "id_results",vals) 
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  # Submit button for species...
  observeEvent(input$buttSearch_species, {
    #info about result type...
    vals$type =  "species"
    #info for output page...
    vals$text <- input$search_key_species
    callModule(session = parent, module = resultsFunc, id = "id_results",vals)
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  # Submit button for genus...
  observeEvent(input$buttSearch_genus, {
    #info about result type...
    vals$type =  "genus"
    #info for output page...
    vals$text <- input$search_key_genus
    callModule(session = parent, module = resultsFunc, id = "id_results",vals)    
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  observe({
      # load SH options...
      updateSelectizeInput(session, "search_key_sh", 
                           choices = global_SH_list, 
                           server = TRUE)
    
      # load species options...
      updateSelectizeInput(session, "search_key_species",
                           choices = global_species_list, 
                           server = TRUE)
      
      # load genus options...
      updateSelectizeInput(session, "search_key_genus", 
                           choices = global_genus_list, 
                           server = TRUE)
      
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