# Function for module UI with SH
resutspHUI <- function(id) {
  ns <- NS(id)
  #pH histograms...
  sidebarPanel(width = "100%", style = "background-color:white;",
    fluidRow(
      column(6,
           wellPanel(
           h2("Histogram of pH of resulting samples"),
           tags$div(id="pHBarStud", style="width:100%;height:200px;"),
           deliverChart(div_id = ns("pHBarStud")),
           style = "background-color:white;",
           h2("Histogram of pH of all samples"),
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
}