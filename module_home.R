# Function for module UI
homeUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;background:url('header.png')",#background:url('search_seq.png')
      fluidRow(
        column(12, style = "height:56px;")
      )
    ),
    # header
    h1(id="welcome_title", paste0("Welcome to ",global_info[,"name"],"!")),
    fluidRow(
      column(12,verbatimTextOutput(ns("fm_info")))
    ),
    # tutorials
    fluidRow(
      column(12, style='padding-left:15px; padding-right:0px; padding-top:0px; padding-bottom:10px',
        actionButton("twitter_butt", label = NULL, 
                             style = "width: 64px; height: 64px; background: url('twitter_butt.png');  background-size: cover; background-position: center;",
                             onclick = paste0("window.open('https://twitter.com/globalfungi')")),
        img(src='none.png', height = 32),     
        "Youtube tutorials:  ",
        img(src='none.png', height = 8),     
             actionButton(ns("buttUse"), label = "How to use it (tutorial)", icon = icon("youtube"), onclick = paste0("window.open('https://www.youtube.com/watch?v=0_opE1hOXwY')")),
        img(src='none.png', height = 8),  
             actionButton(ns("buttSubmit"), label = "How to Submit your Study (tutorial)", icon = icon("youtube"), onclick = paste0("window.open('https://www.youtube.com/watch?v=HmGyr26Hhso')"))
      )
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
    return(cat(paste0(global_info[,"name"], " dataset release ",global_info[,"release"], " (", global_info[,"date"],"). Taxonomy based on UNITE version ",global_info[,"unite_version"], ".\n",
      "Actual number of samples in database ", nrow(global_samples), "; Studies ",nrow(global_papers),".\n",
      "Number of ITS sequence variants ",global_info[,"its_variants_count"], "; Number of ITS1 sequences " , global_info[,"its1_raw_count"],"; Number of ITS2 sequences " , global_info[,"its2_raw_count"],".")
      ))
  })
  
  # popup hint
  # addPopover(session, 
  #            id = ns("map"), 
  #            title = "Sample geolocation", 
  #            content = "Overview of samples geolocations.", 
  #            trigger = "hover")

}