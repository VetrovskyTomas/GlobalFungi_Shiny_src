# User Interface (ui)
fluidPage(
  titlePanel("Basic Map"),
  # Create a new Row in the UI for selectInputs
  sidebarPanel(
    # textInput(inputId = "Spec.Hyp",
    #           label = "Species Hypothesis", 
    #           value = ""),
    selectInput(inputId = "study",
                label = "Studies:",
                choices = c("All", unique(toTrainMetadata$paper_title_year)),
                selected = "All",
                multiple = TRUE,
                selectize = FALSE),
    #uiOutput(outputId = "Samples") this solution didn't work
    selectInput(inputId = "samp.name",
                label = "Samples:",
                choices = toTrainMetadata$sampleHascode,
                multiple = TRUE,
                selectize = FALSE),
    h3("URL components"),
    verbatimTextOutput("urlText")
  ),
  # Create a panel and a set of tabs for the map and the table.
  mainPanel(
    tabsetPanel(
      tabPanel("Map", 
               leafletOutput("plot")),
      tabPanel("Metadata", DT::dataTableOutput("dis"))
    )
  )
)