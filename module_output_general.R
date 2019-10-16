# Function for module UI with SH
outputGeneralSHUI <- function(id) {
  ns <- NS(id)
    #show map & metadata ...
    sidebarPanel(width = "100%", style = "background-color:white;",
      tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
        Shiny.onInputChange(variableName, null);
      });
      "),
      # We MUST load the ECharts javascript library in advance
      loadEChartsLibrary(),
      loadEChartsTheme('shine'),
      # samples info...
      verbatimTextOutput(ns('info_sample_count')),
      # tabs...
      tabsetPanel(id = "tabs",
        tabPanel("SHs",
          br(),
          fluidRow(
            sidebarPanel(
              downloadButton(ns("downloadSHs"), "Download SH list")
            )
          ),
          br(),
          DT::dataTableOutput(ns("SH_list"))  
        ),
      tabPanel("Sample type & Biome",
               # pie charts...
               sidebarPanel(width = "100%", style = "background-color:white;",
                            fluidRow(
                              column(6,                  
                                     h2("Sample type breakdown of resulting samples"),
                                     tags$div(id="typePieStud", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("typePieStud")),
                                     style = "background-color:white;"),
                              column(6,                  
                                     h2("Sample type breakdown of all samples"),
                                     tags$div(id="typePieGlob", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("typePieGlob")),
                                     style = "background-color:white;")
                            ),
                            fluidRow(
                              column(6,      
                                     h2("Sample biome breakdown of resulting samples"),
                                     tags$div(id="biomePieStud", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("biomePieStud")),
                                     style = "background-color:white;"),
                              column(6,               
                                     h2("Sample biome breakdown of all samples"),
                                     tags$div(id="biomePieGlob", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("biomePieGlob")),
                                     style = "background-color:white;")
                            )
               )
          ),
      tabPanel("MAT & MAP",
               #histograms...
               sidebarPanel(width = "100%", style = "background-color:white;",
                            fluidRow(
                              column(6,
                                     h2("Histogram of MAT of resulting samples"),
                                     tags$div(id="MATBarStud", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("MATBarStud")),
                                     style = "background-color:white;"),
                              column(6,
                                     h2("Histogram of MAT of all samples"),
                                     tags$div(id="MATBarGlob", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("MATBarGlob")),
                                     style = "background-color:white;")
                            ),
                            fluidRow(
                              column(6,
                                     h2("Histogram of MAP of resulting samples"),
                                     tags$div(id="MAPBarStud", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("MAPBarStud")),
                                     style = "background-color:white;"),
                              column(6,
                                     h2("Histogram of MAP of all samples"),
                                     tags$div(id="MAPBarGlob", style="width:100%;height:200px;"),
                                     deliverChart(div_id = ns("MAPBarGlob")),
                                     style = "background-color:white;")
                            )
               )
          ),
      tabPanel("pH",
               fluidRow(
                 column(6,
                        h2("Histogram of pH of resulting samples"),
                        tags$div(id="pHBarStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("pHBarStud")),
                        style = "background-color:white;"),
                 column(6,
                        h2("Histogram of pH of all samples"),
                        tags$div(id="pHBarGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("pHBarGlob")),
                        style = "background-color:white;")
               )
      ),
      tabPanel("Geography",
               fluidRow(
                 column(6,
                        h2("Piechart of geolocation of resulting samples"),
                        tags$div(id="geoPieStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("geoPieStud")),
                        style = "background-color:white;"),
                 column(6,
                        h2("Piechart of geolocation of all samples"),
                        tags$div(id="geoPieGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("geoPieGlob")),
                        style = "background-color:white;")
               )
      ),
      tabPanel("Map", 
               leafletOutput(ns("mymap")),
               uiOutput(ns("map_sample_info"))),
      tabPanel("Metadata",
        br(),
        fluidRow(
          column(6,downloadButton(ns("downloadData"), "Download metadata"))
        ),
        br(),
        DT::dataTableOutput(ns("metadata")),
        br(),
        uiOutput(ns("sample_info"))
      )
    )
    )
  
  
}

# Function for module UI with SH
outputGeneralUI <- function(id) {
  ns <- NS(id)
    #show map & metadata ...
    sidebarPanel(width = "100%", style = "background-color:white;",
                 # We MUST load the ECharts javascript library in advance
                 loadEChartsLibrary(),
                 loadEChartsTheme('shine'),
                 # samples info...
                 verbatimTextOutput(ns('info_sample_count')),
                 # tabs...
                 tabsetPanel(id = "tabs",
                             tabPanel("Sample type & Biome",
                                      # pie charts...
                                      sidebarPanel(width = "100%", style = "background-color:white;",
                                                   fluidRow(
                                                     column(6,                  
                                                            h2("Sample type breakdown of resulting samples"),
                                                            tags$div(id="typePieStud", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("typePieStud")),
                                                            style = "background-color:white;"),
                                                     column(6,                  
                                                            h2("Sample type breakdown of all samples"),
                                                            tags$div(id="typePieGlob", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("typePieGlob")),
                                                            style = "background-color:white;")
                                                   ),
                                                   fluidRow(
                                                     column(6,      
                                                            h2("Sample biome breakdown of resulting samples"),
                                                            tags$div(id="biomePieStud", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("biomePieStud")),
                                                            style = "background-color:white;"),
                                                     column(6,               
                                                            h2("Sample biome breakdown of all samples"),
                                                            tags$div(id="biomePieGlob", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("biomePieGlob")),
                                                            style = "background-color:white;")
                                                   )
                                      )
                             ),
                             tabPanel("MAT & MAP",
                                      #histograms...
                                      sidebarPanel(width = "100%", style = "background-color:white;",
                                                   fluidRow(
                                                     column(6,
                                                            h2("Histogram of MAT of resulting samples"),
                                                            tags$div(id="MATBarStud", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("MATBarStud")),
                                                            style = "background-color:white;"),
                                                     column(6,
                                                            h2("Histogram of MAT of all samples"),
                                                            tags$div(id="MATBarGlob", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("MATBarGlob")),
                                                            style = "background-color:white;")
                                                   ),
                                                   fluidRow(
                                                     column(6,
                                                            h2("Histogram of MAP of resulting samples"),
                                                            tags$div(id="MAPBarStud", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("MAPBarStud")),
                                                            style = "background-color:white;"),
                                                     column(6,
                                                            h2("Histogram of MAP of all samples"),
                                                            tags$div(id="MAPBarGlob", style="width:100%;height:200px;"),
                                                            deliverChart(div_id = ns("MAPBarGlob")),
                                                            style = "background-color:white;")
                                                   )
                                      )
                             ),
                             tabPanel("pH",
                                      fluidRow(
                                        column(6,
                                               h2("Histogram of pH of resulting samples"),
                                               tags$div(id="pHBarStud", style="width:100%;height:200px;"),
                                               deliverChart(div_id = ns("pHBarStud")),
                                               style = "background-color:white;"),
                                        column(6,
                                               h2("Histogram of pH of all samples"),
                                               tags$div(id="pHBarGlob", style="width:100%;height:200px;"),
                                               deliverChart(div_id = ns("pHBarGlob")),
                                               style = "background-color:white;")
                                      )
                             ),
                             tabPanel("Geography",
                                      fluidRow(
                                        column(6,
                                               h2("Piechart of geolocation of resulting samples"),
                                               tags$div(id="geoPieStud", style="width:100%;height:200px;"),
                                               deliverChart(div_id = ns("geoPieStud")),
                                               style = "background-color:white;"),
                                        column(6,
                                               h2("Piechart of geolocation of all samples"),
                                               tags$div(id="geoPieGlob", style="width:100%;height:200px;"),
                                               deliverChart(div_id = ns("geoPieGlob")),
                                               style = "background-color:white;")
                                      )
                             ),
                             tabPanel("Map", 
                                      leafletOutput(ns("mymap")),
                                      uiOutput(ns("map_sample_info"))),
                             tabPanel("Metadata",
                                      br(),
                                      fluidRow(
                                        column(6,downloadButton(ns("downloadData"), "Download metadata"))
                                      ),
                                      br(),
                                      DT::dataTableOutput(ns("metadata")),
                                      br(),
                                      uiOutput(ns("sample_info"))
                             )
                 )
    )
}

# Function for module server logic
outputGeneralFunc <- function(input, output, session,  variable, parent) {
  
    #namespace for dynamic input...
    ns <- session$ns
  
    # show output info... 
    output$out_title <- renderText({
      return(paste("Study information page - # samples ", nrow(variable$samples)))
    })
    
    # map of sample from the study...
    output$mymap <- renderLeaflet({
      leaflet(data = variable$samples, options = leafletOptions(preferCanvas = TRUE, minZoom = 1.2)) %>%
        addProviderTiles(providers$Esri, options = providerTileOptions(
          updateWhenZooming = FALSE,  # map won't update tiles until zoom is done
          updateWhenIdle = TRUE)#, noWrap = TRUE) # map won't load new tiles when panning
        ) %>%
        addCircleMarkers(data = variable$samples, ~longitude, ~latitude, layerId = ~id,
                         color = "black",
                         radius = 4,
                         fillColor = "red",
                         weight = 1,
                         stroke = T,
                         fillOpacity = 1#,
        ) 
    })
    
    # pop up text when clicked on map...
    observe({
      click <- input$mymap_marker_click
      if (is.null(click)) {
        print("null click")
        return()
      }
      #print(click)
      text <- paste0(input$mymap_marker_click$id, " ...number of seqs for TAXON / ALL ")
      leafletProxy(mapId = "mymap") %>%
        clearPopups() %>%
        addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
    })
    
    # map sample details - clicked row...
    output$map_sample_info <- renderUI({
      req(length(input$mymap_marker_click) > 0)
        verbatimTextOutput(ns('map_sample_table'))
    })
    
    #
    output$map_sample_table <- renderText({
      sample_vals <- global_samples[which(global_samples$id %in% input$mymap_marker_click$id),]
      #paste0(toString(sample_vals$id)," ",toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
      paste(input$mymap_marker_click$id,toString(global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),"title"]))
    })
    
    # table with samples metadata...
    output$metadata <- DT::renderDataTable({
      variable$samples[,c("id", "primers", "longitude", "latitude", "sample_type", "Biome", "MAT", "MAP", "pH", "year_of_sampling")]
    }, selection = 'single')
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = "sample_list.txt",
      content = function(file) {
        write.table(variable$samples, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
      }
    )
    # render the charts...
    observe({
      if (!is.null(variable$samples)){
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
      
      
      ###########################################
      ############    HISTOGRAMS    #############
      ###########################################
      # get all values from samples and remove NA...
      dat_glob <- global_samples[,"MAT"] %>% filter(global_samples[,"MAT"] != "NA_")
      
      # global bar chart sample type...
      dat <- transform(dat_glob, MAT = as.numeric(MAT))
      dat <- transform(dat, bin = cut(MAT, 10))
      out_sum <- as.data.frame(table(dat$bin))
      # change structure...
      rownames(out_sum) <- out_sum[,1]
      out_sum = subset(out_sum, select = -Var1 )
      row.names(out_sum)<-gsub("\\(", "\\[", row.names(out_sum)) 
      # render...
      renderBarChart(div_id = "MATBarGlob", data = out_sum, theme = "shine",
                     show.legend = FALSE, 
                     direction = "vertical", 
                     font.size.axis.x = 10)
  
      #bar chart sample type...
      dat_study <- variable$samples[,"MAT"] %>% filter(variable$samples[,"MAT"] != "NA_")
      if (nrow(dat_study)>0){
        dat_study <- rbind(dat_glob, dat_study)
        # study bar chart sample type...
        dat <- transform(dat_study, MAT = as.numeric(MAT))
        dat <- transform(dat, bin = cut(MAT, 10))
        out_sum_st <- as.data.frame(table(dat$bin))
        # change structure...
        rownames(out_sum_st) <- out_sum_st[,1]
        out_sum_st = subset(out_sum_st, select = -Var1 )
        row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
        #substract
        out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
        # render...
        renderBarChart(div_id = "MATBarStud", data = out_sum_sub, theme = "shine",
                       show.legend = FALSE, 
                       direction = "vertical", 
                       font.size.axis.x = 10)    
      } else {
        renderGauge(div_id = "MATBarStud", gauge_name = "Data not provided...",
                    rate = 0)
      }
      ############################################################
      # get all values from samples and remove NA...
      dat_glob <- global_samples[,"MAP"] %>% filter(global_samples[,"MAP"] != "NA_")
      
      # global bar chart sample type...
      dat <- transform(dat_glob, MAP = as.numeric(MAP))
      dat <- transform(dat, bin = cut(MAP, 10))
      out_sum <- as.data.frame(table(dat$bin))
      # change structure...
      rownames(out_sum) <- out_sum[,1]
      out_sum = subset(out_sum, select = -Var1 )
      row.names(out_sum)<-gsub("\\(", "\\[", row.names(out_sum)) 
      # render...
      renderBarChart(div_id = "MAPBarGlob", data = out_sum, theme = "shine",
                     show.legend = FALSE, 
                     direction = "vertical", 
                     font.size.axis.x = 10)
      
      #bar chart sample type...
      dat_study <- variable$samples[,"MAP"] %>% filter(variable$samples[,"MAP"] != "NA_")
      if (nrow(dat_study)>0){
        dat_study <- rbind(dat_glob, dat_study)
        # study bar chart sample type...
        dat <- transform(dat_study, MAP = as.numeric(MAP))
        dat <- transform(dat, bin = cut(MAP, 10))
        out_sum_st <- as.data.frame(table(dat$bin))
        # change structure...
        rownames(out_sum_st) <- out_sum_st[,1]
        out_sum_st = subset(out_sum_st, select = -Var1 )
        row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
        #substract
        out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
        # render...
        renderBarChart(div_id = "MAPBarStud", data = out_sum_sub, theme = "shine",
                       show.legend = FALSE, 
                       direction = "vertical", 
                       font.size.axis.x = 10)    
      } else {
        renderGauge(div_id = "MAPBarStud", gauge_name = "Data not provided...",
                    rate = 0)
      }
      
      ############################################################
      # get all values from samples and remove NA...
      dat_glob <- global_samples[,"pH"] %>% filter(global_samples[,"pH"] != "NA_")
      
      # global bar chart sample type...
      dat <- transform(dat_glob, pH = as.numeric(pH))
      dat <- transform(dat, bin = cut(pH, 10))
      out_sum <- as.data.frame(table(dat$bin))
      # change structure...
      rownames(out_sum) <- out_sum[,1]
      out_sum = subset(out_sum, select = -Var1 )
      row.names(out_sum)<-gsub("\\(", "\\[", row.names(out_sum)) 
      # render...
      renderBarChart(div_id = "pHBarGlob", data = out_sum, theme = "shine",
                     show.legend = FALSE, 
                     direction = "vertical", 
                     font.size.axis.x = 10)
      
      #bar chart sample type...
      dat_study <- variable$samples[,"pH"] %>% filter(variable$samples[,"pH"] != "NA_")
      if (nrow(dat_study)>0){
        dat_study <- rbind(dat_glob, dat_study)
        # study bar chart sample type...
        dat <- transform(dat_study, pH = as.numeric(pH))
        dat <- transform(dat, bin = cut(pH, 10))
        out_sum_st <- as.data.frame(table(dat$bin))
        # change structure...
        rownames(out_sum_st) <- out_sum_st[,1]
        out_sum_st = subset(out_sum_st, select = -Var1 )
        row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
        #substract
        out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
        # render...
        renderBarChart(div_id = "pHBarStud", data = out_sum_sub, theme = "shine",
                       show.legend = FALSE, 
                       direction = "vertical", 
                       font.size.axis.x = 10)    
      } else {
        renderGauge(div_id = "pHBarStud", gauge_name = "Data not provided...",
                    rate = 0)
      }
      ############################################################
      
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
      
      }
    })
    
    # sample details - clicked row...
    output$sample_info <- renderUI({
      req(length(input$metadata_cell_clicked) > 0)
      #input$metadata_rows_selected
      wellPanel(
        fluidRow(
          column(4,tableOutput(ns('sample_table_basic'))),
          column(4,tableOutput(ns('sample_table_advance'))),
          column(4,tableOutput(ns('sample_table_paper')), 
                 downloadButton(ns("downloadSeqs"), "Download sequences")
                 )
          ),
        tags$head(tags$style(paste0("#",ns('sample_table_basic')," table {background-color: white; }"), media="screen", type="text/css")),
        tags$head(tags$style(paste0("#",ns('sample_table_advance')," table {background-color: white; }"), media="screen", type="text/css")),
        tags$head(tags$style(paste0("#",ns('sample_table_paper')," table {background-color: white; }"), media="screen", type="text/css"))
        )
    })
    
    #
    output$sample_table_basic <- renderTable({
      sample_vals <- variable$samples[input$metadata_rows_selected,]
      sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), 
        c("id", "longitude", "latitude", "elevation", "MAT", "MAP", 
          "country", "area_sampled", "number_of_subsamples", "sample_depth")]
      sample_vals <- data.frame(basic_metadata = rownames(t(sample_vals)), values = t(sample_vals))
    })
    #
    output$sample_table_advance <- renderTable({
      sample_vals <- variable$samples[input$metadata_rows_selected,]
      sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), 
        c("total_C_content", "total_N_content", "organic_matter_content", 
          "pH", "pH_method", "total_Ca", "total_P", "total_K", "Plants")]
      sample_vals <- data.frame(advance_metadata = rownames(t(sample_vals)), values = t(sample_vals))
    })
    #
    output$sample_table_paper <- renderTable({
      sample_vals <- variable$samples[input$metadata_rows_selected,]
      sample_vals <- global_samples[which(global_samples$id %in% sample_vals$id), ]
      sample_vals <- global_papers[which(global_papers$paper_id %in% sample_vals$paper_id),c("title", "authors", "year","journal", "doi")]
      sample_vals <- data.frame(study = rownames(t(sample_vals)), values = t(sample_vals))
    })
    
    #
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    # table with SHs info...
    if (!is.null(variable$SHs)){
      # show SH list with URL to SH...
      output$SH_list <- DT::renderDataTable(
        DT::datatable({
          data <- variable$SHs[,c("SH","Kingdom","Phylum","Class","Order","Family","Genus","Species")] %>% 
            mutate(SH = paste0("<a href='", "/?SH=",SH,"' target='_blank'>", SH,"</a>"))
          data
        },
        escape = FALSE, selection = 'none')
      )
      # Downloadable csv of selected SHs...
      output$downloadSHs <- downloadHandler(
        filename = "sh_list.txt",
        content = function(file) {
          write.table(variable$SHs, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
        }
      )
    }
    
    # show samples count info...
    output$info_sample_count <- renderText({
      num_samples <- 0
      if (!is.null(variable$samples)){
        num_samples <- nrow(variable$samples)
      }
      return(paste0("Result is covering ", num_samples, " samples")) 
    })
    
}