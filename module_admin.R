# Function for module UI
adminUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    loadEChartsTheme('shine'),    
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='settings.png', height = 56)),
        column(11, h2(id="header_title", "Welcome admin"))
      )
    ),
    # panels...
    sidebarPanel(width = "100%", style = "background-color:white;",
      tabsetPanel(id = ns("navbar"),
        tabPanel("Messages",br(),
          selectInput(ns('select_tab'),'Select table',choice = c("messages")),
          actionButton(ns("refresh_mess"), "Refresh table", icon = icon("redo")),
          br(),
          br(),
          DT::dataTableOutput(ns("table_selected"))                                     
        ),
        tabPanel("Traffic",br(),
          fluidRow(
            column(3,selectInput(ns('select_traffic_type'),'Select traffic type',choice = c("genus","species","SH"))),
            column(6,uiOutput(ns('dynamic_filters'))),
            column(3,actionButton(ns("refresh_traff"), "Refresh traffic", icon = icon("redo")))
          ),
          fluidRow(
            column(3,DT::dataTableOutput(ns("table_data"))),
            column(9,tags$div(id = "graph", style="width: 100%;height:300px;"), deliverChart(div_id = ns("graph")))
          ),
          fluidRow(
            column(3,DT::dataTableOutput(ns("table_data_search"))),
            column(9,tags$div(id = "graph_search", style="width: 100%;height:300px;"), deliverChart(div_id = ns("graph_search")))
          )
        ),
        tabPanel("Submissions",br(),
          actionButton(ns("refresh_studies"), "Refresh table", icon = icon("redo")),
          br(),
          br(),
          DT::dataTableOutput(ns("table_studies")),
          downloadButton(ns("downloadData"), "Download", style = "visibility: hidden;")
        ),
        tabPanel("Data upload", value = "tab_upload",
                 insertUploadUI(id = ns("insert_upload"))
        )
      )
    )
  )
}

# Function for module server logic
adminFunc <- function(input, output, session) {
  
  #namespace for dynamic input...
  ns <- session$ns
  
  # add buttons...
  shinyInput <- function(FUN, len, id, label, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label[i], ...))
    }
    inputs
  }  
  
  # refresh message table...
  observeEvent(input$refresh_mess, {
      output$table_selected <- DT::renderDataTable({
        table <- data.table(sqlQuery(paste0("SELECT * FROM ",input$select_tab," ORDER BY id DESC")))
      })
  })
  
  # refresh studies table...
  observeEvent(input$refresh_studies, {
    output$table_studies <- DT::renderDataTable(
      DT::datatable({
      table <- data.table(sqlQuery(paste0("SELECT * FROM ",options()$mysql$study," ORDER BY date DESC")))
      # set number of samples
      mylist <- c()
      for (i in 1:nrow(table)) {
        query <- paste0("SELECT COUNT(*) FROM ",options()$mysql$metadata," WHERE `paper_study` = '",table[i,"hash"],"'")
        mylist <- c(mylist, sqlQuery(query))
      }
      table$samples <- mylist
      # rest...
      table$paper <- paste(table$title, "<br/>", table$authors, "<br/>", table$journal, " ", table$volume, ": ", table$pages, "(", table$year,")<br/>", table$doi, "<br/>", table$repository)
      table$person <- paste(table$contributor, "<br/>", table$affiliation_institute, "<br/>", table$affiliation_country, "<br/>", table$ORCID,"<br/>", table$email)
      table$collaboration <- paste("include to GFD: ",table$include, "<br/>", "coauthor: ",table$coauthor)
      table$status <- paste("e-mail valid: ",table$email_confirmed, "<br/>", "finished: ",table$submission_finished)
      # metadata button
      table$hash_butt <- ifelse(table$samples>0,shinyInput(actionButton, nrow(table), 'button_', label = table[,"hash"],
                                                           onclick = paste0("Shiny.onInputChange('", ns("lastClickId"), "',this.id);",
                                                                            "Shiny.onInputChange('", ns("lastClick"), "', Math.random())")),table$hash)
      
      table = subset(table, select = c(hash_butt, samples, person, paper, collaboration, status))

      table
      }, escape = FALSE, selection = 'none')
    )
  })
  
  study_hash <- ""
  data <- NULL
  
  # study table button...  
  observeEvent(input$lastClick, {
    index <- as.numeric(strsplit(isolate(input$lastClickId), "_")[[1]][2])
    if (!is.null(index)){
      print("redirect...")
      selectedRow <- index
      print(paste("selectedRow",selectedRow))
      # get metadata table
      table <- data.table(sqlQuery(paste0("SELECT * FROM ",options()$mysql$study," ORDER BY date DESC")))
      study_hash <<- table[selectedRow,"hash"]
      print(paste("hash",study_hash))
      data <<- sqlQuery(paste0("SELECT * FROM ",options()$mysql$metadata," WHERE `paper_study` = '",study_hash,"'"))
      shinyjs::runjs(paste0("document.getElementById('",ns("downloadData"),"').click();"))
    }
  }, ignoreInit = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data-", study_hash, ".csv")
    },
    content = function(file) {
      # combine it with basic info
      table <- data.table(sqlQuery(paste0("SELECT * FROM ",options()$mysql$study," WHERE `hash` = '",study_hash,"'")))

      data <- cbind(study_year = unlist(rep(table[,"year"],times=nrow(data))),data)
      
      data <- cbind(doi = unlist(rep(table[,"doi"],times=nrow(data))),data)
      data <- cbind(pages = unlist(rep(table[,"pages"],times=nrow(data))),data)
      data <- cbind(volume = unlist(rep(table[,"volume"],times=nrow(data))),data)
      data <- cbind(journal = unlist(rep(table[,"journal"],times=nrow(data))),data)
      data <- cbind(title = unlist(rep(table[,"title"],times=nrow(data))),data)
      
      data <- cbind(include_as_collab = unlist(rep(table[,"include"],times=nrow(data))),data)
      data <- cbind(group_author = unlist(rep(table[,"coauthor"],times=nrow(data))),data)
      
      data <- cbind(ORCID = unlist(rep(table[,"ORCID"],times=nrow(data))),data)
      data <- cbind(aff_country = unlist(rep(table[,"affiliation_country"],times=nrow(data))),data)
      data <- cbind(aff_institute = unlist(rep(table[,"affiliation_institute"],times=nrow(data))),data)
      
      data <- cbind(email = unlist(rep(table[,"email"],times=nrow(data))),data)
      data <- cbind(contributor = unlist(rep(table[,"contributor"],times=nrow(data))),data)
      # write it...
      write.csv2(data, file, row.names = FALSE, dec = ".", sep = "\t", quote = FALSE)
    }
  )
  
  # dynamic filters for traffic...
  output$dynamic_filters <- renderUI({
    date_from <- sqlQuery(paste0("SELECT `date` FROM ",options()$mysql$traffic," ORDER BY date ASC LIMIT 1;"))
    date_to <- sqlQuery(paste0("SELECT `date` FROM ",options()$mysql$traffic," ORDER BY date DESC LIMIT 1;"))
    
    date_from <- sqlQuery(paste0("SELECT DATE('",date_from,"')"))
    date_to <- sqlQuery(paste0("SELECT DATE('",date_to,"')"))
    
    date_to <- as.Date.character(date_to) + 1
    
    print(paste0("Date from ",date_from," to ",date_to))
    dateRangeInput(ns("daterange"), "Date range:", start = date_from, end = date_to)
  })  
  
  # refresh traffic charts...
  observeEvent(input$refresh_traff, {
    traffic_type <- isolate(input$select_traffic_type)
    daterange <- isolate(input$daterange)
    # taxonomy search overview...
    bar_data <- sqlQuery(
      paste0("SELECT * FROM ",options()$mysql$traffic," WHERE date >= '",daterange[1],"' AND date <= '",daterange[2],"' AND `category` = '",traffic_type,"'")
    )
    if (nrow(bar_data)>0){
      #prepare data...
      dat <- as.data.frame(table(bar_data[,"value"]))
      colnames(dat) <- c(traffic_type, "value")
      # get table...
      output$table_data <- DT::renderDataTable({
        table <- dat
      })
      colnames(dat) <- c("name", "value")
      #renderPieChart(div_id = "graph", data = dat, radius = "60%",center_x = "50%", center_y = "50%", show.legend = FALSE)
      renderPieChart(div_id = "graph", data = dat, show.legend = FALSE)
    } else {
      renderGauge(div_id = "graph", gauge_name = "No data within selected range...",rate = 0)
    }
    # search overview...
    dat_category <- sqlQuery(
      paste0("SELECT `category` FROM ",options()$mysql$traffic," WHERE date >= '",daterange[1],"' AND date <= '",daterange[2],"'")
    )
    if (nrow(dat_category)>0){
      dat_category <- as.data.frame(table(dat_category[,"category"]))
      #prepare data...
      #dat <- as.data.frame(table(bar_data[,"value"]))
      colnames(dat_category) <- c("category", "value")
      # get table...
      output$table_data_search <- DT::renderDataTable({
        table <- dat_category
      })
      colnames(dat_category) <- c("name", "value")
      renderPieChart(div_id = "graph_search", data = dat_category, show.legend = FALSE)
    } else {
      renderGauge(div_id = "graph_search", gauge_name = "No data within selected range...",rate = 0)
    }
  })
  
  # TEST
  # create storage for study info
  study <- reactiveValues()
  study$valid <- TRUE
  study$info <- NULL
  study$basic <- NULL
  study$metadata <- NULL
  study$correct <- FALSE
  study$start <- FALSE
  observe({
    callModule(module = insertUploadFunc, id = "insert_upload", study)  
  })
  
}