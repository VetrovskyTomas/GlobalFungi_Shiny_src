```
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("leaflet")
# install.packages("DT")
# install.packages("ECharts2Shiny")
# install.packages("readxl")

# install.packages("devtools")
# library(devtools)
# install.packages("remotes")
# library(remotes)
# install_github("RedOakStrategic/geoshaper")

# install.packages("RMySQL")
# install.packages("markdown")
# install.packages("mapview")
# install.packages("readxl")

# install.packages('shinyFiles')
# install.packages('leaflet.extras')
# install.packages('shinyBS')

# install.packages('mailR')
# install.packages("shinycssloaders")
# install.packages("future")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(plyr)
library(dplyr)
library(data.table) # fast read
library(leaflet) # interactive world map
library(leaflet.extras)
library(DT)
library(ECharts2Shiny)
library(geoshaper)
#library(shinycssloaders)
#library(future)

setwd("C:/Users/koste/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/")
runApp(appDir = "FM_DATABASE_SRC/")

#runApp(appDir = "AMF_DATABASE_SRC/")
#setwd("C:/Users/avetrot/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/")
#setwd("C:/Users/Eliska/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/")
# runApp(appDir = "/srv/shiny-server", launch.browser = F)


# AMF DATABAZE
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plyr)
library(dplyr)
library(data.table) # fast read
library(leaflet) # interactive world map
library(DT)
library(ECharts2Shiny)
library(geoshaper)
runApp(appDir = "/home/ubuntu/AMF_DATABASE_SRC", launch.browser = F)

runApp(appDir = "/home/ubuntu/Docker_GlobalFungi/app/", launch.browser = F)


#runApp(appDir = "C:/Users/Kocomour/Dropbox/GRANT_METASTUDY_410344/DATABASE_WEB/SHINY_DASHBOARD_EXAMPLES/mobile_detect/mobileDetect-master/")

###############################
# INSTALL rJava #
##########################
#sudo apt-get install -y default-jre
#sudo apt-get install -y default-jdk
#sudo R CMD javareconf
#sudo apt-get install r-cran-rjava
```
