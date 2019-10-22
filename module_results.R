# Function for module UI
resultsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    # title...
    h2(id="welcome_title",textOutput(ns("out_title"))),
    # paper info table...
    sidebarPanel(width = "100%", style = "background-color:#f8f8f8;",
      uiOutput(ns('dynamic_header'))
    ),
    # all panels...
    sidebarPanel(id = ns("panel"), width = "100%", style = "background-color:white;",
                 # We MUST load the ECharts javascript library in advance
                 loadEChartsLibrary(),
                 loadEChartsTheme('shine'),
                 # samples info...
                 verbatimTextOutput(ns('info_sample_count')),
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
resultsFunc <- function(input, output, session, variable, parent) {
  # namespace for dynamic input...
  ns <- session$ns

  # show title...
  output$out_title <- renderText({
    num_samples <- 0
    if (!is.null(out_data$samples)){
      num_samples <- nrow(out_data$samples)
    }
    return(paste0("Here are the results for ", variable$type, " covering ", num_samples, " samples")) 
  })
  
  out_data <- reactiveValues()
  
  # render the charts...
  observe({
    #################################################################
    # default
    out_data$samples <- NULL
    out_data$SHs <- NULL
    out_data$SeqVars <- NULL
    
    if (variable$text != "") {
      # study option...
      if (variable$type == "study"){
        # filter papers data...
        output$info_table <- renderTable({
          global_papers[which(global_papers$paper_id %in% variable$text),c("title", "authors", "year","journal", "doi", "contact")]
        })
        # filer sample based on selection...
        out_data$samples <- global_samples[which(global_samples$paper_id %in% variable$text),]
      } else
        # sequence option...  
        if (variable$type == "sequence") {
          withProgress(message = 'Searching...', {
          # search by md5 checksum...
          variants <- global_variants[which(global_variants$hash %in% variable$key),]
          #print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
          if (nrow(variants) > 0){
            # get info table...
            if (variants$SH != "-"){
              output$info_table <- renderTable({
                sh_data <- global_SH[which(global_SH$SH %in% variants$SH),]
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
            samples <- strsplit(variants$samples, ';', fixed=TRUE)
            samples <- unique(unlist(samples))
            print(paste0("Number of samples in ",variable$text," is ", length(samples)))
            
            out_data$samples <- global_samples[which(global_samples$id %in% samples),]
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
          if (variable$type == "SH") {
            # filter papers data...
            output$info_table <- renderTable({
              global_SH[which(global_SH$SH %in% variable$text),]
            })
            # filer sample based on selection...
            variants <- global_variants[which(global_variants$SH %in% variable$text),]
            print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
            out_data$SeqVars <- variants[,c("samples", "hash", "marker")]
            
            samples <- strsplit(variants$samples, ';', fixed=TRUE)
            samples <- unique(unlist(samples))
            print(paste0("Number of samples in ",variable$text," is ", length(samples)))
            
            out_data$samples <- global_samples[which(global_samples$id %in% samples),]
          } else
            # species option...  
            if (variable$type == "species") {
              # get all SH based on species...
              SH_list <- global_SH[which(global_SH$Species %in% variable$text),]
              print(paste0("Number of SH in ",variable$text," is ", nrow(SH_list)))
              # pass the list of SHs...
              out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
              # filter species data...
              output$info_table <- renderTable({
                tax_tab <- out_data$SHs[1,c("Species", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus")]
              })
              # filer sample based on selection...
              variants <- global_variants[which(global_variants$SH %in% SH_list$SH),]
              print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
              out_data$SeqVars <- variants[,c("samples", "hash", "marker")]
              
              samples <- strsplit(variants$samples, ';', fixed=TRUE)
              samples <- unique(unlist(samples))
              print(paste0("Number of samples in ",variable$text," is ", length(samples)))
              
              out_data$samples <- global_samples[which(global_samples$id %in% samples),]
            } else
              # genus option...  
              if (variable$type == "genus") {
                # get all SH based on genus...
                SH_list <- global_SH[which(global_SH$Genus %in% variable$text),]
                print(paste0("Number of SH in ",variable$text," is ", nrow(SH_list)))
                # pass the list of SHs...
                out_data$SHs <- global_SH[which(global_SH$SH %in% SH_list$SH),]
                # filter genus data...
                output$info_table <- renderTable({
                  tax_tab <- out_data$SHs[1,c("Genus","Kingdom", "Phylum", "Class", "Order", "Family")]
                })
                # filer sample based on selection...
                variants <- global_variants[which(global_variants$SH %in% SH_list$SH),]
                print(paste0("Number of seqs in ",variable$text," is ", nrow(variants)))
                out_data$SeqVars <- variants[,c("samples","hash", "marker")]
                
                samples <- strsplit(variants$samples, ';', fixed=TRUE)
                samples <- unique(unlist(samples))
                print(paste0("Number of samples in ",variable$text," is ", length(samples)))
                
                out_data$samples <- global_samples[which(global_samples$id %in% samples),]
              }    
    
    #################################################################
    
    if (!is.null(out_data$samples)){
      ############################################################      
      callModule(module = resutsTypesAndBiomesFunc, id = "results_types_biomes", out_data)
      callModule(module = resutsMatMapFunc, id = "results_matmap", out_data)
      callModule(module = resutspHFunc, id = "results_ph", out_data)
      callModule(module = resutsGeographyFunc, id = "results_geography", out_data)
      callModule(module = resutsMapFunc, id = "results_map", out_data)
      callModule(module = resutsSamplesFunc, id = "results_samples", out_data)
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
      ############################################################
    }
    # variants...
    if (!is.null(out_data$SeqVars)){
      callModule(module = resutsVariantsFunc, id = "results_variants", out_data, variable)
    }
      
    }
  })
  
  # dynamic header - seq vars...
  output$dynamic_header <- renderUI({
    if (is.null(out_data$samples)){
      h2("No results found.")
    } else {
      if (!is.null(out_data$SeqVars)){
        fluidRow(
          column(10,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table'))),
          resutsVariantsUI(id = ns("results_variants"))
        )
      } else {
        fluidRow(
          column(12,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table')))
        )
      }
    }
  })
  
  # key
  output$info_key <- renderText(
    if (variable$type == "study"){
      toString(global_papers[which(global_papers$paper_id %in% variable$text),"title"])
    } else { 
      variable$text
    }
  )
  
  # hide or show panel with tabs...
  observe({
    if (is.null(out_data$samples)){
      hide(id = "panel")
    } else {
      show(id = "panel")
    }
  })
  
  # show samples count info...
  output$info_sample_count <- renderText({
    num_samples <- 0
    if (!is.null(out_data$samples)){
      num_samples <- nrow(out_data$samples)
    }
    return(paste0("Result is covering ", num_samples, " samples")) 
  })
  
}