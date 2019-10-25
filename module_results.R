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
resultsFunc <- function(input, output, session, variable) {
  # namespace for dynamic input...
  ns <- session$ns

  type <- isolate(variable$type)
  text <- isolate(variable$text)
  key <- isolate(variable$key)
  singl <- isolate(variable$single)
  
  # render the charts...
  observe({
    ################################################################
    out_data <- reactiveValues()
    # default
    out_data$samples <- NULL
    out_data$SHs <- NULL
    out_data$SeqVars <- NULL
    
    # study option...
      if (variable$type == "study"){
        withProgress(message = 'Searching...', {
          # filter papers data...
          incProgress(1/3)
          output$info_table <- renderTable({
            global_papers[which(global_papers$paper_id %in% text),c("title", "authors", "year","journal", "doi", "contact")]
          })
          # filer sample based on selection...
          incProgress(1/3)
          out_data$samples <- global_samples[which(global_samples$paper_id %in% text),]
        })
      } else
        # sequence option...  
        if (type == "sequence") {
          withProgress(message = 'Searching...', {
            # search by md5 checksum...
            incProgress(1/3)
            variants <- global_variants[which(global_variants$hash %in% key),]
            #print(paste0("Number of seqs in ",text," is ", nrow(variants)))
            incProgress(1/3)
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
              
              # filter by sigleton option...
              if (singl == FALSE){
                sample_tab <- sample_tab[sample_tab$abundances > 1,]
                if (nrow(sample_tab) == 0) {
                  sample_tab <- NULL
                }
              }
              
              # fill out data...
              out_data$samples <- sample_tab
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
              # filter papers data...
              incProgress(1/3)
              output$info_table <- renderTable({
                global_SH[which(global_SH$SH %in% text),]
              })
              # filer sample based on selection...
              incProgress(1/3)
              variants <- global_variants[which(global_variants$SH %in% text),]
              print(paste0("Number of seqs in ",text," is ", nrow(variants)))
              out_data$SeqVars <- variants[,c("samples", "hash", "marker")]
              
              incProgress(1/3)
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
              
              # filter by sigleton option...
              if (singl == FALSE){
                sample_tab <- sample_tab[sample_tab$abundances > 1,]
                if (nrow(sample_tab) == 0) {
                  sample_tab <- NULL
                }
              }
              
              # fill out data...
              out_data$samples <- sample_tab
            })
          } else
            # species option...  
            if (type == "species") {
              withProgress(message = 'Searching...', {
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
                variants <- global_variants[which(global_variants$SH %in% SH_list$SH),]
                print(paste0("Number of seqs in ",text," is ", nrow(variants)))
                out_data$SeqVars <- variants[,c("samples", "hash", "marker")]
                
                samples <- strsplit(variants$samples, ';', fixed=TRUE)
                abundances <- strsplit(variants$abundances, ';', fixed=TRUE)
                # store counts...
                cover <- data.frame(sample = unlist(samples), abund = as.numeric(unlist(abundances)), stringsAsFactors = F)
                cover <- ddply(cover,~sample,summarise,abundance=sum(abund))
                
                # continue with list of samples...
                samples <- unique(unlist(samples))
                incProgress(1/5)
                print(paste0("Number of samples in ",text," is ", length(samples)))
                
                # append coverage
                sample_tab <- global_samples[which(global_samples$id %in% samples),]
                sample_tab$abundances <- cover$abundance[match(sample_tab$id, cover$sample)]
                
                # filter by sigleton option...
                if (singl == FALSE){
                  sample_tab <- sample_tab[sample_tab$abundances > 1,]
                  if (nrow(sample_tab) == 0) {
                    sample_tab <- NULL
                  }
                }
                
                # fill out data...
                out_data$samples <- sample_tab
                
              })
            } else
              # genus option...  
              if (type == "genus") {
                withProgress(message = 'Searching...', {
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
                  variants <- global_variants[which(global_variants$SH %in% SH_list$SH),]
                  print(paste0("Number of seqs in ",text," is ", nrow(variants)))
                  out_data$SeqVars <- variants[,c("samples","hash", "marker")]
                  
                  samples <- strsplit(variants$samples, ';', fixed=TRUE)
                  abundances <- strsplit(variants$abundances, ';', fixed=TRUE)
                  # store counts...
                  cover <- data.frame(sample = unlist(samples), abund = as.numeric(unlist(abundances)), stringsAsFactors = F)
                  cover <- ddply(cover,~sample,summarise,abundance=sum(abund))

                  # continue with list of samples...
                  samples <- unique(unlist(samples))
                  incProgress(1/5)
                  print(paste0("Number of samples in ",text," is ", length(samples)))
                  
                  # append coverage
                  sample_tab <- global_samples[which(global_samples$id %in% samples),]
                  sample_tab$abundances <- cover$abundance[match(sample_tab$id, cover$sample)]
                  
                  # filter by sigleton option...
                  if (singl == FALSE){
                    sample_tab <- sample_tab[sample_tab$abundances > 1,]
                    if (nrow(sample_tab) == 0) {
                      sample_tab <- NULL
                    }
                  }
                  
                  # fill out data...
                  out_data$samples <- sample_tab
                })
              }    
    
    #################################################################
    if (!is.null(out_data$samples)){
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
    }
    ###################################################################
    # variants...
    if (!is.null(out_data$SeqVars)){
      isolate(
      callModule(module = resutsVariantsFunc, id = "results_variants", out_data, type, text)
      )
    }

    
    # dynamic header - seq vars...
    output$dynamic_header <- renderUI({
      if (is.null(out_data$samples)){
        h2(paste("No results found. ( ignore singletons ",!singl,")"))
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
              resutsVariantsUI(id = ns("results_variants"))
            )
          } else {
            fluidRow(
              column(10,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table'))),
              resutsVariantsUI(id = ns("results_variants"))
            )
          }
          ###################################
        } else {
          fluidRow(
            column(12,verbatimTextOutput(ns('info_key')),tableOutput(ns('info_table')))
          )
        }
      }
    })
    
    # # button to unite
    # output$unite_butt <- renderText(
    #   if (type == "study"){
    #     toString(global_papers[which(global_papers$paper_id %in% text),"title"])
    #   } else { 
    #     text
    #   }
    # )
    
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
        num_samples <- nrow(out_data$samples)
      }
      return(paste0("Result is covering ", num_samples, " samples")) 
    })
    
    # hide or show panel with tabs...
    observe({
      print("hide or show panel")
      if (is.null(out_data$samples)){
        hide(id = "panel")
      } else {
        show(id = "panel")
      }
    })
    
    # show title...
    output$out_title <- renderText({
      num_samples <- 0
      if (!is.null(out_data$samples)){
        num_samples <- nrow(out_data$samples)
      }
      return(paste0("Here are the results for ", type, " covering ", num_samples, " samples")) 
    })
  })
  
  
  
}