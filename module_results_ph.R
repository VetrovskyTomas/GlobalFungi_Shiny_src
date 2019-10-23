# Function for module UI with SH
resutspHUI <- function(id) {
  ns <- NS(id)
  #pH histograms...
  sidebarPanel(width = "100%", style = "background-color:white;",
    fluidRow(
      column(12,
           wellPanel(
           h2(textOutput(ns("na_count_sel"))),
           tags$div(id="pHBarSel", style="width:100%;height:200px;"),
           deliverChart(div_id = ns("pHBarSel")),
           style = "background-color:white;",
           h2(textOutput(ns("na_count_glob"))),
           tags$div(id="pHBarGlob", style="width:100%;height:200px;"),
           deliverChart(div_id = ns("pHBarGlob")),
           style = "background-color:white;"
           )
      )
    )
  )
}

# Function for module server logic
resutspHFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  

  counts <- reactiveValues()
  counts$sel_obs <- 0
  counts$glob_obs <- 0
  
  observe({  
    # get all values from samples and remove NA...
    dat_glob <- global_samples[,"pH"] %>% filter(global_samples[,"pH"] != "NA_")
    counts$glob_obs <- nrow(dat_glob) 
    
    # global bar chart sample type...
    dat <- transform(dat_glob, pH = as.numeric(pH))
    dat <- transform(dat, bin = cut(pH, 20))
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
    dat_sel <- variable$samples[,"pH"] %>% filter(variable$samples[,"pH"] != "NA_")
    counts$sel_obs <- nrow(dat_sel)
    if (nrow(dat_sel)>0){
      dat_sel <- rbind(dat_glob, dat_sel)
      # sel bar chart sample type...
      dat <- transform(dat_sel, pH = as.numeric(pH))
      dat <- transform(dat, bin = cut(pH, 20))
      out_sum_st <- as.data.frame(table(dat$bin))
      # change structure...
      rownames(out_sum_st) <- out_sum_st[,1]
      out_sum_st = subset(out_sum_st, select = -Var1 )
      row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
      #substract
      out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
      # render...
      renderBarChart(div_id = "pHBarSel", data = out_sum_sub, theme = "shine",
                     show.legend = FALSE, 
                     direction = "vertical", 
                     font.size.axis.x = 10)    
    } else {
      renderGauge(div_id = "pHBarSel", gauge_name = "Data not provided...",
                  rate = 0)
    }
  })
  
  # get info about NA in selection...
  output$na_count_sel <- renderText({
    return(paste0("Histogram of pH from selected samples (", counts$sel_obs, " out of ",nrow(variable$samples)," total observations)"))
  })
  # get info about NA all samples...
  output$na_count_glob <- renderText({
    return(paste0("Histogram of pH of all samples (", counts$glob_obs, " out of ",nrow(global_samples)," total observations)"))
  })
}