
### Clear workspace (optional)
#rm(list = ls())

### Expand java memory (helps with Rstox)
#options(java.parameters = "-Xmx20000m")

#### 1. Install and load packages ####
packages <- c("tidyverse","rgeos","rgdal","sp","mapdata","marmap","mapplots","gridExtra",
              "lme4","devtools","mgcv","glmmTMB","jtools","data.table","sjstats",
              "RstoxBase","RstoxData","ggOceanMaps","ggOceanMapsData", "knitr", "kableExtra",
              "emmeans","effects","ggpubr","boot","raster","scales","mapproj")

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  if("RstoxBase" %in% packages[!installed_packages])  {
    install.packages("RstoxBase", repos = c("https://stoxproject.github.io/repo", "https://cloud.r-project.org"))## Install Rstox
  } else if ("RstoxData" %in% packages[!installed_packages] ) {
    install.packages("RstoxData", repos = c("https://stoxproject.github.io/repo/", "https://cloud.r-project.org/")) ## Install RstoxData
  } else if ("ggOceanMaps" %in% packages[!installed_packages]) {
    devtools::install_github("MikkoVihtakari/ggOceanMapsData") # Install ggOceanMaps
    devtools::install_github("MikkoVihtakari/ggOceanMaps")
  } else {
    install.packages(packages[!installed_packages],repos="https://cloud.r-project.org")
  }
}
sapply(packages, require, character.only = TRUE)


#### 2. Source helper functions ####
# 
# load_funs <- paste0("_source/",list.files(path="_source",pattern=".R"))
# 
# sapply(load_funs, FUN= source)
# 
