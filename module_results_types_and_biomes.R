# Function for module UI with SH
resutsTypesAndBiomesUI <- function(id) {
  ns <- NS(id)
  # types and biomes pie charts...
  sidebarPanel(width = "100%", style = "background-color:white;",
               fluidRow(
                 column(6,    
                        wellPanel(
                        h2("Sample type breakdown of resulting samples"),
                        tags$div(id="typePieStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("typePieStud")),
                        style = "background-color:white;",
                        h2("Sample type breakdown of all samples"),
                        tags$div(id="typePieGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("typePieGlob")),
                        style = "background-color:white;"
                        )
               ),
                 column(6,      
                        wellPanel(
                        h2("Sample biome breakdown of resulting samples"),
                        tags$div(id="biomePieStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("biomePieStud")),
                        style = "background-color:white;",
                        h2("Sample biome breakdown of all samples"),
                        tags$div(id="biomePieGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("biomePieGlob")),
                        style = "background-color:white;"
                        )
               )
    )
  )
}

# Function for module server logic
resutsTypesAndBiomesFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  if (nrow(variable$samples)>0){  
    #pie chart sample type...
    datS <- as.data.frame(table(variable$samples[,"sample_type"]))
    colnames(datS) <- c("name", "value")
    # render...
    renderPieChart(div_id = "typePieStud", data = datS, show.legend = FALSE, radius = "75%")
    
    # global pie chart sample type...
    datG <- as.data.frame(table(global_samples[,"sample_type"]))
    colnames(datG) <- c("name", "value")
    # sort names to have the same color in pie chart...
    x <- as.character(datG$name) %in% datS$name
    datG <- rbind(datG[x,], datG[!x,])
    # render...
    renderPieChart(div_id = "typePieGlob", data = datG, show.legend = FALSE, radius = "75%")  

    #pie chart of samples biome...
    datS <- as.data.frame(table(variable$samples[,"Biome"]))
    colnames(datS) <- c("name", "value")
    # render...
    renderPieChart(div_id = "biomePieStud", data = datS, show.legend = FALSE, radius = "75%")
    
    # global pie chart biome...
    datG <- as.data.frame(table(global_samples[,"Biome"]))
    colnames(datG) <- c("name", "value")
    # sort names to have the same color in pie chart...
    x <- as.character(datG$name) %in% datS$name
    datG <- rbind(datG[x,], datG[!x,])
    # render...
    renderPieChart(div_id = "biomePieGlob", data = datG, show.legend = FALSE, radius = "75%")
  } else {
    renderGauge(div_id = "typePieStud", gauge_name = "No results...", rate = 0)
    renderGauge(div_id = "typePieGlob", gauge_name = "No results...", rate = 0)
    renderGauge(div_id = "biomePieStud", gauge_name = "No results...", rate = 0)
    renderGauge(div_id = "biomePieGlob", gauge_name = "No results...", rate = 0)
  }
}