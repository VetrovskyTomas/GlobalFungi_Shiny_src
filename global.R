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

library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet) # interactive world map
library(DT)
library(ECharts2Shiny)
library(plyr)
library(lazyeval) # so we can use interpret
library(dplyr)
library(data.table) # fast read
library(markdown) # bring text data from markdown

##############
### GLOBAL ###
##############
users = reactiveValues(count = 0, max = 0, IPs = vector())
# load module for data
source("module_load.R", local = TRUE)

# load module functions
source("module_home.R")
source("module_search.R")
source("module_insert.R")
source("module_analysis.R")
source("module_studies.R")
source("module_cite.R")
source("module_help.R")
source("module_aboutus.R")

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
