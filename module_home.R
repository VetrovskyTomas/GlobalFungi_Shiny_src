# Function for module UI
homeUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(12, style = "background-color:#0c2b37;",
          img(src='search_seq.png', height = 56),
          img(src='search_tax.png', height = 56),
          img(src='help.png', height = 56),
          img(src='cite.png', height = 56),
          img(src='geosearch.png', height = 56),
          img(src='admin.png', height = 56),
          img(src='info.png', height = 56),
          img(src='insert.png', height = 56),
          img(src='task.png', height = 56),
          img(src='studies.png', height = 56),
          img(src='message.png', height = 56),
          img(src='about.png', height = 56),
          img(src='aboutus.png', height = 56),
          img(src='collaborators.png', height = 56)
        )
      )
    ),
    # header
    h1(id="welcome_title", paste0("Welcome to ",global_info[,"name"],"!")),
    fluidRow(
      column(12,verbatimTextOutput(ns("fm_info")))
    ),
    # basic info...
    sidebarPanel(width = "100%", style = "background-color:white;",
                fluidRow(
                  column(8,leafletOutput(ns("map"), height=500)),
                  column(4,                  
                         tags$div(id="contPie", style="width:100%;height:500px;"),
                         deliverChart(div_id = ns("contPie")),
                         style = "background-color:white;")
                )
    )
  )
}

# Function for module server logic
homeFunc <- function(input, output, session, samples) {
  #namespace for dynamic input...
  ns <- session$ns
  
  output$map <- renderLeaflet({
    leaflet(data = global_samples, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
      addProviderTiles(providers$Esri, options = providerTileOptions(
        updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
        updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
      ) %>%
      addCircleMarkers(data = global_samples, ~longitude, ~latitude, options = pathOptions(clickable = FALSE),
                       color = "black",
                       radius = 3,
                       fillColor = "red",
                       weight = 1,
                       stroke = T,
                       fillOpacity = 0.5#, 
      ) 
  })
  
  observe({
    #pie chart continents...
    dat <- as.data.frame(table(global_samples[,"continent"]))
    colnames(dat) <- c("name", "value")
    renderPieChart(div_id = "contPie", data = dat, radius = "60%",center_x = "50%", center_y = "50%", show.legend = FALSE)
  })
  
  #basic info...
  output$fm_info <- renderPrint({
    # create output text
    return(cat(paste0("Database version ",global_info[,"version"], "; Release version ", global_info[,"release"],"; Unite version ",global_info[,"unite_version"], "; Last update ", global_info[,"date"],"; ", global_info[,"info"],".\n",
      "Actual number of samples in database ", nrow(global_samples), "; Studies ",nrow(global_papers),".\n",
      "Number of ITS sequence variants ",global_variants_count, "; Number of ITS1 sequences " , global_info[,"its1_raw_count"],"; Number of ITS2 sequences " , global_info[,"its2_raw_count"],".")
      ))
  })
  
  # popup hint
  # addPopover(session, 
  #            id = ns("map"), 
  #            title = "Sample geolocation", 
  #            content = "Overview of samples geolocations.", 
  #            trigger = "hover")

}