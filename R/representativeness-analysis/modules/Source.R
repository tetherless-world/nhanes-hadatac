#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")

library(BiocManager)
options(repos = BiocManager::repositories())


##### Load packages ####
# dependencies <- c("shinydashboard" , "shiny" , "shinyBS" , "ggplot2" , 
#                   "methods" , "hexbin" , "DT" , "dplyr" , "RColorBrewer" , 
#                   "BBmisc" , "viridis" , "hrbrthemes" , "plotly" , "tidyr" , 
#                   "dqshiny" , "haven", "survey", "stringr", "interactions", 
#                   "gplots", "lazyeval", "tidyverse", "treemap", "sunburstR", "d3r", "rintrojs", "jsonlite", 
#                   "sp", "shinyjqui", "data.table", "formattable", "kableExtra", "DataCombine", "knitr", "lpsymphony", 
#                   "bsplus", "gtools", "rAmCharts", "shinyjs", "shinyWidgets", "rjson")

#install.dependencies <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
# Check and install packages not yet available
#if (length(install.dependencies) > 0) {
  #install.packages(install.dependencies)
#}




# Load all packages
library(shinydashboard)
library(shiny)
library(shinyBS)
library(ggplot2)
library(methods)
library(hexbin)
library(DT)
library(dplyr)
#library(d3heatmap)
library(RColorBrewer)
library(BBmisc)
library(viridis)
library(hrbrthemes)
library(plotly)
library(tidyr)
library(dqshiny)
library(haven)
library(survey)
library(stringr)
library(interactions)
library(gplots)
library(lazyeval)
library(tidyverse)
library(treemap)
library(sunburstR)
library(d3r)
library(rintrojs)
library(jsonlite)
library(sp)
library(shinyjqui)
#library(geofacet)
#library(minimap)
library(gtsummary)
library(data.table)
library(formattable)
library(kableExtra)
library(DataCombine)
library(knitr)
library(magrittr)
#library(Rsolnp)
library(lpsymphony)
library(bsplus)
library(gtools)
library(rAmCharts)
library(shinyjs)
library(shinyWidgets)
library(rjson)
library(shinyFeedback)
