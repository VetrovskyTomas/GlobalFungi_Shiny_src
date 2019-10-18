# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("leaflet")
# install.packages("DT")
# install.packages("ECharts2Shiny")
# install.packages("markdown")

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
users = reactiveValues(count = 0, max = 0)
# load module for data
source("module_load.R", local = TRUE)

# load module functions
source("module_home.R")
source("module_search.R")
source("module_insert.R")
source("module_analysis_test.R")
source("module_studies.R")
source("module_cite.R")
source("module_help.R")
source("module_aboutus.R")
# outputs...
source("module_output.R")
source("module_output_general.R")
