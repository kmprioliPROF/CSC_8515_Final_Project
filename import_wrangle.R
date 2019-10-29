# Katherine M. Prioli
# CSC 8515 Final Project
# Tue Oct 29 09:27:15 2019 ------------------------------


#### Loading libraries ----

library(tidyverse)    # For the usual reasons
library(SASxport)     # For loading SAS .xpt files
library(forcats)      # For handling categorical data
library(gridExtra)    # For grid.arrange()
library(grid)         # For textGrob() to annotate grid.arrange() elements
library(kableExtra)   # For prettifying output tables
library(ggthemr)      # For prettifying output plots
library(rmarkdown)    # For `render()`

ggthemr("fresh")


#### Importing datasets ----

# NHANES 2015-2016 data (Source:  https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015)

demog_raw <- read.xport("data/DEMO_I.XPT")     # NHANES 2015-2016 demographics data
diabetes_raw <- read.xport("data/DIQ_I.XPT")   # NHANES 2015-2016 questionnaire data - diabetes items


#### Wrangling data ----



#### Sending data to .Rmd ----

# render("analysis_report_nb.Rmd")