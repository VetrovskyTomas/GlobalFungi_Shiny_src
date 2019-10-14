# Function for module UI
homeUI <- function(id) {
  ns <- NS(id) # Creates Namespace
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    #picture...
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(12, style = "background-color:#0c2b37;",img(src='myImage.png', align = "left"))
      )
    ),
    #header
    h1(id="welcome_title", "Welcome to fungal metastudy database!"),
    verbatimTextOutput(ns("fm_info")),
    #basic info...
    textOutput(ns("sample_counts")),
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
  # When user clicks on "Hello" button : Update reactive variable "name"
  getSum <- eventReactive(input$var, {
    return(summary(global_samples[,as.numeric(input$var)]))
  })
  #basic info...
  output$fm_info <- renderPrint({
    return(cat(paste0("Actual number of samples in database: ", nrow(global_samples), " (",nrow(global_papers)," studies)")))
    #return("Actual number of samples in database: ")
  })
  
  output$map <- renderLeaflet({
    leaflet(data = global_samples, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
      addProviderTiles(providers$Esri, options = providerTileOptions(
        updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
        updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
      ) %>%
      addCircleMarkers(data = global_samples, ~longitude, ~latitude,
                       color = "black",
                       radius = 3,
                       fillColor = "red",
                       weight = 1,
                       stroke = T,
                       fillOpacity = 0.5#,
      ) 
  })
    
  #BAR chart continents...
  output$contBar <- renderPlot({
    tab <- as.data.frame(table(global_samples[,"sample_type"]))
    Plot <- ggplot(data = tab, aes_string(x = tab$Var1, y = tab$Freq, fill = tab$Var1)) + geom_bar(stat = "identity", position=position_dodge())
    Plot <- Plot + ylab("# of samples")
    Plot <- Plot + theme_bw() + theme(panel.border = element_blank(), text =  element_text(face = "bold", size = 12), axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 5, 3, 0), "mm"), angle = 90), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black")) + ggtitle(paste(input$data_type," vs # of samples"))
    Plot
  })
    
  observe({
    #pie chart continents...
    dat <- as.data.frame(table(global_samples[,"continent"]))
    colnames(dat) <- c("name", "value")
    renderPieChart(div_id = "contPie", data = dat, radius = "60%",center_x = "50%", center_y = "50%", show.legend = FALSE)
  })
  
  #
  output$sum <- renderPrint({
    getSum()
  })
  
  output$box <- renderPlot({
    x<-getSum()
    boxplot(x,col="sky blue",border="purple",main=names(global_samples[as.numeric(input$var)]))
  })

}