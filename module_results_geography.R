# Function for module UI with SH
resutsGeographyUI <- function(id) {
  ns <- NS(id)
  #geography histograms...
  sidebarPanel(width = "100%", style = "background-color:white;",
  fluidRow(
    column(6,
           wellPanel(
           h2("Piechart of geolocation of resulting samples"),
           tags$div(id="geoPieStud", style="width:100%;height:200px;"),
           deliverChart(div_id = ns("geoPieStud")),
           style = "background-color:white;",
           h2("Piechart of geolocation of all samples"),
           tags$div(id="geoPieGlob", style="width:100%;height:200px;"),
           deliverChart(div_id = ns("geoPieGlob")),
           style = "background-color:white;"
           )
      )
    )
  )
}

# Function for module server logic
resutsGeographyFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  if (nrow(variable$samples)>0){   
    # pie chart of samples geography...
    datS <- as.data.frame(table(variable$samples[,"continent"]))
    colnames(datS) <- c("name", "value")
    
    # render...
    renderPieChart(div_id = "geoPieStud", data = datS, show.legend = FALSE, radius = "75%")
    
    # global pie chart biome...
    datG <- as.data.frame(table(global_samples[,"continent"]))
    colnames(datG) <- c("name", "value")
    # sort names to have the same color in pie chart...
    x <- as.character(datG$name) %in% datS$name
    datG <- rbind(datG[x,], datG[!x,])
    # render...
    renderPieChart(div_id = "geoPieGlob", data = datG, show.legend = FALSE, radius = "75%")
  } else {
    renderGauge(div_id = "geoPieStud", gauge_name = "No results...", rate = 0)
    renderGauge(div_id = "geoPieGlob", gauge_name = "No results...", rate = 0)
  }
}