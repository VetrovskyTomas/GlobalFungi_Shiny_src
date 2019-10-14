# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("leaflet")
# install.packages("DT")
# install.packages("ECharts2Shiny")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table) # fast read
library(leaflet) # interactive world map
library(DT)
library(ECharts2Shiny)

##############
### GLOBAL ###
##############
setwd("C:/Users/Kocomour/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/SHINY_EXAMPLES/")
#setwd("C:/Users/avetrot/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/SHINY_EXAMPLES/")
#setwd("C:/Users/Eliska/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/SHINY_EXAMPLES/")
main_session <- NULL

if(!exists("global_samples")) {
  # load samples table...
  global_samples <- fread("C:/fm_database_root/tables/fm_samples_v7.txt")
  
  # construct papers table...
  global_papers <- global_samples[,c("paper_id", "title_year", "authors", "journal", "doi", "contact")]
  global_papers <- distinct(global_papers, paper_id, .keep_all= TRUE) # remove duplicate rows based on variable
  # split title and year...
  splited_title_year <- do.call('rbind', strsplit(as.character(global_papers$title_year), '_', fixed=TRUE))
  colnames(splited_title_year) <- c("title", "year")
  global_papers <- cbind(global_papers, splited_title_year)
  global_papers = subset(global_papers, select = -c(title_year) ) #drop column...
  
  # filter sample table...
  global_samples <- global_samples[,c("id", "paper_id", "sample_type", "latitude", "longitude", "continent", 
                                      "year_of_sampling", "Biome", "sequencing_platform", "target_gene", "primers", 
                                      "elevation", "MAT", "MAP", "country", "Plants", "area_sampled", "number_of_subsamples", "sample_depth", 
                                      "total_C_content", "total_N_content", "organic_matter_content", 
                                      "pH", "pH_method", "total_Ca", "total_P", "total_K")]
  
  # load SH table...
  global_SH <- fread("C:/fm_database_root/tables/fm_sh_07FU.txt")
  global_SH <- global_SH[,c("SH", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
  
  # load sequence variants with SH...
  global_variants <- fread("C:/fm_database_root/tables/fm_sequences_test.txt")
  global_variants <- global_variants[,c("sequence", "samples", "abundances", "SH")]
  
  # remove SH not existing in the dataset...
  SH_list <- unique(global_variants$SH)
  global_SH <- global_SH %>% filter(SH %in% SH_list)
}


# load module functions
source("module_home.R")
source("module_search.R")
source("module_insert.R")
source("module_analysis.R")
source("module_studies.R")
source("module_cite.R")
source("module_help.R")
# outputs...
source("module_output.R")
source("module_output_general.R")
