# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("leaflet")
# install.packages("DT")
# install.packages("ECharts2Shiny")
# install.packages("markdown")
# install.packages("seqRFLP")
# install.packages("shinyalert")

library(leaflet.extras)
library(ECharts2Shiny)
library(plyr)
library(data.table)
library(shiny)
library(RMySQL)
library(digest)
library(shinydashboard)
library(stringr)
library(leaflet)
library(markdown)
library(readxl)
library(shinyBS)
library(DT)
library(lazyeval)
library(stringi)
library(shinyjs)
library(sp)
library(dplyr)
library(geoshaper)
library(mailR)

##############
### GLOBAL ###
##############
users = reactiveValues(count = 0, max = 0, IPs = vector())
# load module for data
source("module_load.R", local = TRUE)

# load module functions
source("module_home.R")
source("module_search.R")
source("module_analysis.R")
source("module_clusters.R")
source("module_studies.R")
source("module_cite.R")
source("module_help.R")
source("module_aboutus.R")
source("module_join.R")
source("module_message.R")
source("module_geoshape.R")
source("module_collaborators.R")

# insert
source("module_insert.R")
source("module_insert_intro.R")
source("module_insert_basicinfo.R")
source("module_insert_metadata.R")
source("module_insert_upload.R")

# hidden 
source("module_admin.R")
source("module_admin_analysis.R")

# general results page 
source("module_results.R")
# sub-results
source("module_results_matmap.R")
source("module_results_ph.R")
source("module_results_geography.R")
source("module_results_types_and_biomes.R")
source("module_results_map.R")
source("module_results_sample.R")
source("module_results_samples.R")
source("module_results_shs.R")
source("module_results_variants.R")
