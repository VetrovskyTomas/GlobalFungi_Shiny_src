# Function for module UI with SH
resutsMatMapUI <- function(id) {
  ns <- NS(id)
  
  #MAT & MAP histograms...
  sidebarPanel(width = "100%", style = "background-color:white;",
               fluidRow(
                 column(6,
                        wellPanel(
                        h2("Histogram of MAT of resulting samples"),
                        tags$div(id="MATBarStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("MATBarStud")),
                        style = "background-color:white;",
                        h2("Histogram of MAT of all samples"),
                        tags$div(id="MATBarGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("MATBarGlob")),
                        style = "background-color:white;"
                        )
               ),
                 column(6,
                       wellPanel(
                        h2("Histogram of MAP of resulting samples"),
                        tags$div(id="MAPBarStud", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("MAPBarStud")),
                        style = "background-color:white;",
                        h2("Histogram of MAP of all samples"),
                        tags$div(id="MAPBarGlob", style="width:100%;height:200px;"),
                        deliverChart(div_id = ns("MAPBarGlob")),
                        style = "background-color:white;"
                        )
               )
  )
  )
}

# Function for module server logic
resutsMatMapFunc <- function(input, output, session,  variable) {
  
  #namespace for dynamic input...
  ns <- session$ns  
  
  ####################################
  ############    MAT    #############
  ####################################    
  # get all values from samples and remove NA...
  dat_glob <- global_samples[,"MAT"] %>% filter(global_samples[,"MAT"] != "NA_")
  
  # global bar chart sample type...
  dat <- transform(dat_glob, MAT = as.numeric(MAT))
  dat <- transform(dat, bin = cut(MAT, breaks=c(-12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 100)))
  
  out_sum <- as.data.frame(table(dat$bin))
  # change structure...
  rownames(out_sum) <- out_sum[,1]
  out_sum = subset(out_sum, select = -Var1 )
  #row.names(out_sum)<-gsub("\\(", "\\[", row.names(out_sum))
  row.names(out_sum)<- c("..-8","-8,-4","-4,0","0,4","4,8","8,12","12,16","16,20","20,24","24..")
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
    dat <- transform(dat, bin = cut(MAT, breaks=c(-12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 100)))
    out_sum_st <- as.data.frame(table(dat$bin))
    # change structure...
    rownames(out_sum_st) <- out_sum_st[,1]
    out_sum_st = subset(out_sum_st, select = -Var1 )
    #row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
    row.names(out_sum_st)<- c("..-8","-8,-4","-4,0","0,4","4,8","8,12","12,16","16,20","20,24","24..")
    #substract
    out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
    # render...
    renderBarChart(div_id = "MATBarStud", data = out_sum_sub, theme = "shine",
                   show.legend = FALSE, 
                   direction = "vertical", 
                   font.size.axis.x = 10)    
  } else {
    renderGauge(div_id = "MATBarStud", gauge_name = "Data not provided...", rate = 0)
  }
  
  ####################################
  ############    MAP    #############
  ####################################  
  # get all values from samples and remove NA...
  dat_glob <- global_samples[,"MAP"] %>% filter(global_samples[,"MAP"] != "NA_")
  
  # global bar chart sample type...
  dat <- transform(dat_glob, MAP = as.numeric(MAP))
  dat <- transform(dat, bin = cut(MAP, breaks=c(0, 50, 100, 200, 400, 600, 800, 1000, 2000, 4000, 10000)))
  out_sum <- as.data.frame(table(dat$bin))
  # change structure...
  rownames(out_sum) <- out_sum[,1]
  out_sum = subset(out_sum, select = -Var1 )
  #row.names(out_sum)<-gsub("\\(", "\\[", row.names(out_sum))
  row.names(out_sum) <- c("0-50","50-100","100-200","200-400","400-600","600-800","800-1000","1000-2000","2000-4000","4000..") 
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
    dat <- transform(dat, bin = cut(MAP, breaks=c(0, 50, 100, 200, 400, 600, 800, 1000, 2000, 4000, 10000)))
    out_sum_st <- as.data.frame(table(dat$bin))
    # change structure...
    rownames(out_sum_st) <- out_sum_st[,1]
    out_sum_st = subset(out_sum_st, select = -Var1 )
    #row.names(out_sum_st)<-gsub("\\(", "\\[", row.names(out_sum_st))
    row.names(out_sum_st) <- c("0-50","50-100","100-200","200-400","400-600","600-800","800-1000","1000-2000","2000-4000","4000..") 
    #substract
    out_sum_sub <- out_sum_st-out_sum[colnames(out_sum_st)]
    # render...
    renderBarChart(div_id = "MAPBarStud", data = out_sum_sub, theme = "shine",
                   show.legend = FALSE, 
                   direction = "vertical", 
                   font.size.axis.x = 10)
  } else {
    renderGauge(div_id = "MAPBarStud", gauge_name = "Data not provided...", rate = 0)
  }
}