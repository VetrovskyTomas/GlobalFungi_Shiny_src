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
            label = "Select SH:", choices = NULL, width = "200px",
            selected = NULL, multiple = FALSE, # allow for multiple inputs
            options = list(maxOptions = 5)
          ),
          fluidRow(
            column(6, actionButton(ns("buttSearch_sh"), label = "Search"))
          )
        ),
        tabPanel("Species", 
          br(),
          selectizeInput(inputId = ns("search_key_species"),
                         label = "Select SH:", choices = NULL, width = "400px",
                         selected = NULL, multiple = FALSE, # allow for multiple inputs
                         options = list(maxOptions = 5)
          ),
          fluidRow(
            column(6, actionButton(ns("buttSearch_species"), label = "Search"))
          )
        ),
        tabPanel("Genus",
          br(),  
          selectizeInput(inputId = ns("search_key_genus"),
            label = "Select genus:", choices = NULL, width = "300px",
            selected = NULL, multiple = FALSE, # allow for multiple inputs
            options = list(maxOptions = 5)
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
    
    callModule(session = parent, module = outputFunc, id = "id_results",vals, parent = parent)    
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  # Submit button for species...
  observeEvent(input$buttSearch_species, {
    #info about result type...
    vals$type =  "species"
    #info for output page...
    vals$text <- input$search_key_species
    
    callModule(session = parent, module = outputFunc, id = "id_results",vals, parent = parent)    
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  # Submit button for genus...
  observeEvent(input$buttSearch_genus, {
    #info about result type...
    vals$type =  "genus"
    #info for output page...
    vals$text <- input$search_key_genus
    
    callModule(session = parent, module = outputFunc, id = "id_results",vals, parent = parent)    
    updateTabItems(session = parent, "menu_tabs", "fmd_results")
  })
  
  observe({
      updateSelectizeInput(session, "search_key_sh", choices = global_SH$SH, server = TRUE)
      species <- unique(global_SH$Species)
      updateSelectizeInput(session, "search_key_species", choices = sort(species), server = TRUE)
      genera <- unique(global_SH$Genus)
      updateSelectizeInput(session, "search_key_genus", choices = sort(genera), server = TRUE)
      # make bar chart...
      bar_data <- data.frame(
        name = c("SH","Species","Genus"),
        value = c(length(global_SH$SH),length(species),length(genera))
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