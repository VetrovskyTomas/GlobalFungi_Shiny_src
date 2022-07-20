# Function for module UI
resultsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
                 fluidRow(
                   column(1, style = "background-color:#0c2b37;",img(src='results.png', height = 56)),
                   column(11, h2(id="header_title", textOutput(ns("out_title"))))
                 )
    ),
    # general info panel...
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
      uiOutput(ns('dynamic_header')),
      hr(),
      uiOutput(ns('dynamic_filters'))
    ),
    # all panels...
    sidebarPanel(id = ns("panel"), width = "100%", style = "background-color:white;",
                 # We MUST load the ECharts javascript library in advance
                 loadEChartsLibrary(),
                 loadEChartsTheme('shine'),
                 # filtered results...
                 verbatimTextOutput(ns('info_sample_filtered')),
                 # tabs...
                 tabsetPanel(id = ns("tabs"),
                             tabPanel("SHs", value = "tab_SH",
                                      resutsSHsUI(id = ns("results_shs"))
                             ),
                             tabPanel("Sample type & Biome", value = "tab_biomes",
                                      resutsTypesAndBiomesUI(id = ns("results_types_biomes"))
                             ),
                             tabPanel("MAT & MAP",
                                      resutsMatMapUI(id = ns("results_matmap"))
                             ),
                             tabPanel("pH",
                                      resutspHUI(id = ns("results_ph"))
                             ),
                             tabPanel("Geography",
                                      resutsGeographyUI(id = ns("results_geography"))
                             ),
                             tabPanel("Map", 
                                      resutsMapUI(id = ns("results_map"))
                             ),
                             tabPanel("Metadata",
                                      resutsSamplesUI(id = ns("results_samples"))
                             )
                 )
    )
  )
}

# Function for module server logic
resultsFunc <- function(input, output, session, variable) {
  # namespace for dynamic input...
  ns <- session$ns
  
  type <- isolate(variable$type)
  text <- isolate(variable$text)
  key <- isolate(variable$key)
  if (is.null(key)||(key=="")){
    key <- text
  }

  # tracking traffic...
  tracking_traffic <- function(category, value) {
    query <- paste0("INSERT INTO ",options()$mysql$traffic, 
      " (session, category, value) VALUES (",global_session,", '", category, "', '", value, "')")
    sqlQuery(query)
  }
  
  # get samples tab funtion...
  sample_tab <- function(variants) {
    samples <- strsplit(variants$samples, ';', fixed=TRUE)
    abundances <- strsplit(variants$abundances, ';', fixed=TRUE)
    # store counts...
    cover <- data.frame(sample = unlist(samples), abund = as.numeric(unlist(abundances)), stringsAsFactors = F)
    cover <- ddply(cover,~sample,summarise,abundance=sum(abund))
    
    # continue with list of samples...
    samples <- unique(unlist(samples))
    print(paste0("Number of samples in ",text," is ", length(samples)))
    
    # append coverage
    sample_tab <- global_samples[which(global_samples$id %in% samples),]
    sample_tab$abundances <- cover$abundance[match(sample_tab$id, cover$sample)]
    
    sample_tab
  }  
   
  # render the charts...
  observe({
    ################################################################
    out_data <- reactiveValues()
    # default
    out_data$samples <- NULL
    out_data$SHs <- NULL
    out_data$SeqVars <- NULL

    # study option...
      if (type == "study"){
        withProgress(message = 'Searching...', {
          # tracking...
          tracking_traffic(type, text)
          
          # filter papers data...
          incProgress(1/3)
          output$info_table <- renderTable({
            paper_tab <- global_papers[which(global_papers$paper_id %in% text),c("title", "authors", "year","journal", "doi", "contact")]
            names(paper_tab) <- c("Title", "Authors", "Year","Journal", "DOI", "Contact")
            paper_tab
          })
          # filer sample based on selection...
          incProgress(1/3)
          out_data$samples <- global_samples[which(global_samples$paper_id %in% text),]
        })
      } else
        # sequence option...  
        if ((type == "sequence")||(type == "single-blast")||(type == "multi-blast")) {
          withProgress(message = 'Searching...', {
            # search by md5 checksum...
            key_string <- paste0("('",paste(key, collapse="','" ),"')")
            # Construct the fetching query
            query <- paste0("SELECT `hash`,`samples`,`abundances`,`SH`,`marker`,`sequence` from ",options()$mysql$variants_table," WHERE `hash` IN ",key_string)
            variants <- sqlQuery(query)
            
            incProgress(1/3)
            
            if (nrow(variants) > 0) {
              # tracking...
              tracking_traffic(type, length(variants$SH))
              # get SH info table...
              if (length(variants$SH) > 1){
                # group of variants
                key <- "group"
                shs <- unique(variants$SH)
                shs <- shs[which(shs != 0)]
                print(shs)
                if (length(shs) > 0){
                  out_data$SHs <- global_SH[which(global_SH$SH_id %in% shs),]
                  output$info_table <- renderTable({
                    bar_data <- data.frame(
                      SH = c(paste0(length(shs)," SH detected - see SH list below"))
                    )
                  })
                } else {
                  output$info_table <- renderTable({
                    bar_data <- data.frame(
                      SH = c("-")
                    )
                  }) 
                }
              } else {
                # single variant
                if (variants$SH != 0){
                  output$info_table <- renderTable({
                    sh_data <- global_SH[which(global_SH$SH_id %in% variants$SH),c("SH", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
                    sh_data$SH <- paste0("<a href='", "/?SH=",sh_data$SH,"' target='_blank'>", sh_data$SH,"</a>")
                    data.frame(sh_data)
                  }, sanitize.text.function = function(x) x)
                } else {
                  output$info_table <- renderTable({
                    bar_data <- data.frame(
                      SH = c("-")
                    )
                  })
                }
              }
              incProgress(1/3)
              
              # get samples table...
              print(paste0("Number of seqs in ",text," is ", nrow(variants)))
              
              out_data$SeqVars <- variants[,c("hash","samples","abundances", "marker", "sequence")]
              #print(out_data$SeqVars)
              incProgress(1/3)
              out_data$samples <- sample_tab(variants)
            }
            else
            {
              output$info_table <- renderTable({
                bar_data <- data.frame(
                  status = c("No hit!")
                )
              })
            }
          })
        } else
          # SH option...  
          if (type == "SH") {
            withProgress(message = 'Searching...', {
              # tracking...
              tracking_traffic(type, text)
              # filter papers data...
              incProgress(1/3)
              output$info_table <- renderTable({
                global_SH[which(global_SH$SH %in% text),c("SH", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
              })
              # filer sample based on selection...
              incProgress(1/3)
              
              # Construct the fetching query
              query <- sprintf(paste0("SELECT * from ",options()$mysql$sh_table," WHERE `sh` = '",text,"'"))
              tax_out <- sqlQuery(query)
              
              # result is not empty...
              if (nrow(tax_out) > 0) {
                print(paste0("Number of seqs in ",text," is ", tax_out$vars))
                out_data$SeqVars <- data.frame(vars = tax_out$vars, stringsAsFactors = F)
                incProgress(1/5)
                out_data$samples <- sample_tab(tax_out)
              }
            })
          } else
            # species option...  
            if (type == "species") {
              withProgress(message = 'Searching...', {
                # tracking...
                tracking_traffic(type, text)
                # get all SH based on species...
                incProgress(1/5)
                SH_list <- global_SH[which(global_SH$Species %in% text),]
                print(paste0("Number of SH in ",text," is ", nrow(SH_list)))
                # pass the list of SHs...
                incProgress(1/5)
                out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
                # filter species data...
                incProgress(1/5)
                output$info_table <- renderTable({
                  tax_tab <- out_data$SHs[1,c("Species", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus")]
                })
                # filer sample based on selection...
                incProgress(1/5)
                
                
                # Construct the fetching query
                query <- sprintf(paste0("SELECT * from ",options()$mysql$species_table," WHERE `species` = '",text,"'"))
                tax_out <- sqlQuery(query)
                
                # result is not empty...
                if (nrow(tax_out) > 0) {
                  print(paste0("Number of seqs in ",text," is ", tax_out$vars))
                  out_data$SeqVars <- data.frame(vars = tax_out$vars, stringsAsFactors = F)
                  incProgress(1/5)
                  out_data$samples <- sample_tab(tax_out)
                }
              })
            } else
              # genus option...  
              if (type == "genus") {
                withProgress(message = 'Searching...', {
                  # tracking...
                  tracking_traffic(type, text)
                  # get all SH based on genus...
                  incProgress(1/5)
                  SH_list <- global_SH[which(global_SH$Genus %in% text),]
                  print(paste0("Number of SH in ",text," is ", nrow(SH_list)))
                  # pass the list of SHs...
                  incProgress(1/5)
                  out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
                  # filter genus data...
                  incProgress(1/5)
                  output$info_table <- renderTable({
                    tax_tab <- out_data$SHs[1,c("Genus","Kingdom", "Phylum", "Class", "Order", "Family")]
                  })
                  # filer sample based on selection...
                  incProgress(1/5)
                  
                  # Construct the fetching query
                  query <- sprintf(paste0("SELECT * from ",options()$mysql$genus_table," WHERE `genus` = '",text,"'"))
                  tax_out <- sqlQuery(query)
                  
                  # result is not empty...
                  if (nrow(tax_out) > 0) {
                    print(paste0("Number of seqs in ",text," is ", tax_out$vars))
                    out_data$SeqVars <- data.frame(vars = tax_out$vars, stringsAsFactors = F)
                    incProgress(1/5)
                    out_data$samples <- sample_tab(tax_out)
                  }
                })
              }
    #################################################################
    if (!is.null(out_data$samples)){
      filtered_data <- reactiveValues()
      filtered_data$filter <- TRUE
      filtered_data$samples <- isolate(out_data$samples[out_data$samples$manipulated != "true",])
      
      callModule(module = resutsTypesAndBiomesFunc, id = "results_types_biomes", filtered_data)
      callModule(module = resutsMatMapFunc, id = "results_matmap", filtered_data)
      callModule(module = resutspHFunc, id = "results_ph", filtered_data)
      callModule(module = resutsGeographyFunc, id = "results_geography", filtered_data)
      callModule(module = resutsMapFunc, id = "results_map", filtered_data)
      callModule(module = resutsSamplesFunc, id = "results_samples", filtered_data)
      ####
      # table with SHs info...
      if (!is.null(out_data$SHs)){
        callModule(module = resutsSHsFunc, id = "results_shs", out_data)
        print("Show SHs panel...")
        showTab(inputId = "tabs", target = "tab_SH")
        updateTabsetPanel(session, "tabs", selected = "tab_SH")
      } else {
        print("Hide SHs panel...")
        hideTab(inputId = "tabs", target = "tab_SH")
        updateTabsetPanel(session, "tabs", selected = "tab_biomes")
      }
    }
    ###################################################################
    # variants...
    if (!is.null(out_data$SeqVars)){
      isolate(
        callModule(module = resutsVariantsFunc, id = "results_variants", out_data, type, key)
      )
    }

    
    # dynamic header - seq vars...
    output$dynamic_header <- renderUI({
      if (is.null(out_data$samples)){
        h2(paste("No results found."))
      } else {
        if (!is.null(out_data$SeqVars)){
          ###################################
          if (type == "SH") {
            fluidRow(
              column(10,
                fluidRow(  
                  column(10,verbatimTextOutput(ns('info_key'))),
                  column(2,actionButton(inputId = ns("unite_butt"), 
                      label = NULL, 
                      style = "width: 120px; height: 40px;background: url('unite.png');  background-size: cover; background-position: center;",
                      onclick = paste0("window.open('https://unite.ut.ee/bl_forw_sh.php?sh_name=",text,"', '_blank')")
                    ))
                ),
              column(10,tableOutput(ns('info_table')))),
              resutsVariantsUI(id = ns("results_variants")),
              column(12,verbatimTextOutput(ns('info_sample_count')))
            )
          } else {
            fluidRow(
              column(10,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table'))),
              resutsVariantsUI(id = ns("results_variants")),
              column(12,verbatimTextOutput(ns('info_sample_count')))
            )
          }
          ###################################
        } else {
          fluidRow(
            column(12,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table'))),
            column(12,verbatimTextOutput(ns('info_sample_count')))
          )
        }
      }
    })
    
    # show filtered samples count info...
    output$info_sample_filtered <- renderText({
      num_samples <- 0
      if (!is.null(out_data$samples)){
        num_samples <- nrow(filtered_data$samples)
      }
      return(paste0("Filtered result is covering ", num_samples, " samples (NO FILTERS APPLIED)"  ))
    })

    
    # apply filters...
    observeEvent(input$applyFilters, {
      if (!is.null(out_data$samples)){
        withProgress(message = 'Filtering...', {
          filtered_data <- reactiveValues()
          filtered_data$filter <- TRUE
          filtered_data$samples <- isolate(out_data$samples)
          print(nrow(filtered_data$samples))
          # filter by singletons...
          if (length(input$sample_single) > 0) {
            filtered_data$samples <- filtered_data$samples[filtered_data$samples$abundances > 1,]
          }
          # filter by manipulation...
          if (length(input$sample_manipulated) == 0) {
            filtered_data$samples <- filtered_data$samples[filtered_data$samples$manipulated != "true",]
          }
          # filter by sample type...
          filtered_data$samples <- filtered_data$samples[which(filtered_data$samples$sample_type %in% input$sample_type),]
          # filter by sample biome...
          filtered_data$samples <- filtered_data$samples[which(filtered_data$samples$Biome %in% input$sample_biome),]
          # filter by sample year...
          years <- isolate(input$sample_year)
          if (length(years)>1){
            years <- c(years[1]:years[2])
          }
          if ((length(input$sample_year_NA)>0)&&(input$sample_year_NA == TRUE)){
            years <- c(years,NA)
          }
          filtered_data$samples <- filtered_data$samples[which(as.numeric(filtered_data$samples$year_of_sampling) %in% years),]
          
          # show filtered samples count info...
          output$info_sample_filtered <- renderText({
            isolate({
              filter <- input$sample_year
              if (length(input$sample_year)>1){
                filter <- paste(c(input$sample_year[1]:input$sample_year[2]), collapse=",")
              }
              if (length(input$sample_year_NA)>0){
                filter <- paste0("Sampling years: ",filter," (NA included ", input$sample_year_NA,")")
              } else {
                filter <- paste0("Sampling years: ",filter)
              }
              filter <- paste("Sample biomes:",paste(input$sample_biome, collapse=","),"\n",filter)
              filter <- paste("Sample types:",paste(input$sample_type, collapse=","),"\n",filter)
              if (length(input$sample_single) > 0) {
                filter <- paste("Singletons ignored: TRUE\n",filter)
              } else {
                filter <- paste("Singletons ignored: FALSE\n",filter)
              }
              if (length(input$sample_manipulated) > 0) {
                filter <- paste("Manipulated samples add: TRUE\n",filter)
              } else {
                filter <- paste("Manipulated samples add: FALSE\n",filter)
              }
              return(paste0("Filtered result is covering ", nrow(filtered_data$samples), " samples - selected filters:\n", filter))
            })
          })
          #
          if (nrow(filtered_data$samples) > 0){
            shinyjs::show(id = "panel")
            # apply...
            callModule(module = resutsTypesAndBiomesFunc, id = "results_types_biomes", filtered_data)
            callModule(module = resutsMatMapFunc, id = "results_matmap", filtered_data)
            callModule(module = resutspHFunc, id = "results_ph", filtered_data)
            callModule(module = resutsGeographyFunc, id = "results_geography", filtered_data)
            callModule(module = resutsMapFunc, id = "results_map", filtered_data)
            callModule(module = resutsSamplesFunc, id = "results_samples", filtered_data)
            if (!is.null(out_data$SHs)){
              filtered_data$SHs <- isolate(out_data$SHs)
              callModule(module = resutsSHsFunc, id = "results_shs", out_data)
            }
          } else {
            shinyjs::hide(id = "panel")
          }
        })
      }
    }, ignoreInit = TRUE)
    
    # dynamic filters...
    output$dynamic_filters <- renderUI({
      if (!is.null(out_data$samples)){
        num_man <- length(out_data$samples$manipulated[out_data$samples$manipulated=="true"])
        # variables...
        sample_years <- as.numeric(out_data$samples$year_of_sampling)
        year_na <- NA %in% sample_years
        sample_years=sample_years[!is.na(sample_years)]
        sample_biomes <- sort(unique(out_data$samples$Biome))
        sample_types <- sort(unique(out_data$samples$sample_type))
        # filters...
        fluidRow(
          column(2,checkboxGroupInput(ns("sample_single"), 
                                      "Ignore singletons:",
                                      choiceNames = "ignore",
                                      choiceValues = "ignore",
                                      selected = ""
          ),
          if (num_man>0){
          checkboxGroupInput(ns("sample_manipulated"), 
                            paste0("Add manipulated studies (",length(out_data$samples$manipulated[out_data$samples$manipulated=="true"]),") :"),
                             choiceNames = "add",
                             choiceValues = "add",
                             selected = ""
          )}
          ),
          column(2,checkboxGroupInput(ns("sample_biome"), 
                                      "Filter biome:",
                                      choiceNames = sample_biomes, 
                                      choiceValues = sample_biomes,
                                      selected = sample_biomes
          ) 
          ),
          column(2,checkboxGroupInput(ns("sample_type"), 
                                      "Filter type:",
                                      choiceNames = sample_types, 
                                      choiceValues = sample_types,
                                      selected = sample_types
          ) 
          ),
          if (length(sample_years) > 0) {
            size <- 4
            if (year_na) {
              size <- 3
            }
            column(size,sliderInput(ns("sample_year"), "Sampling year:",
                                 min = min(sample_years), max = max(sample_years), value = c(min(sample_years),max(sample_years)), step = 1, sep = "")
            )
          },
          if (year_na) {
            size <- 1
            if (length(sample_years) == 0) {
              size <- 2
            }
            column(size,
            if (length(sample_years) == 0) {
              HTML(paste("<b>Sampling year:</b>"))
            },
            checkboxInput(ns("sample_year_NA"), "include NA", value = TRUE, width = NULL))
          }
          ,column(2,actionButton(ns("applyFilters"), "Apply filters", icon = icon("filter")),img(src='filter.png', align = "left"))
        )
      }
    })
    
    # key
    output$info_key <- renderText(
      if (type == "study"){
        toString(global_papers[which(global_papers$paper_id %in% text),"title"])
      } else { 
        text
      }
    )
    
    # show samples count info...
    output$info_sample_count <- renderText({
      num_samples <- 0
      if (!is.null(out_data$samples)){
        num_samples <- length(out_data$samples$manipulated[out_data$samples$manipulated=="false"])
      }
      return(paste0("Original result is covering ", num_samples, " samples")) 
    })
    
    # hide or show panel with tabs...
    observe({
      print("hide or show panel")
      if (is.null(out_data$samples)){
        shinyjs::hide(id = "panel")
        shinyjs::hide(id = "filters")
      } else {
        shinyjs::show(id = "panel")
        shinyjs::show(id = "filters")
      }
    })
    
    # show title...
    output$out_title <- renderText({
      num_samples <- 0
      if (!is.null(out_data$samples)){
        num_samples <- length(out_data$samples$manipulated[out_data$samples$manipulated=="false"])
      }
      return(paste0("Here are the results for ", type, " covering ", num_samples, " samples")) 
    })
  })
  
  
  
}