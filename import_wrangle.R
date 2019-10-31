# Katherine M. Prioli
# CSC 8515 Final Project
# Thu Oct 31 19:42:04 2019 ------------------------------


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

dietbehav_raw <- read.xport("data/DBQ_I.XPT")    # Questionnaire data - dietary behavior
dietintake_raw <- read.xport("DR1TOT_I.XPT")     # Dietary intake data, Day 1
PHQ9_raw <- read.xport("data/DPQ_I.XPT")         # Questionnaire data - PHQ-9
bloodpress_raw <- read.xport("data/BPX_I.XPT")   # Examination data - blood pressure
bodymeas_raw <- read.xport("data/BMX_I.XPT")     # Examination data - body measures
demog_raw <- read.xport("data/DEMO_I.XPT")       # Demographics data
diabetes_raw <- read.xport("data/DIQ_I.XPT")     # Questionnaire data - diabetes items
medical_raw <- read.xport("data/MCQ_I.XPT")      # Questionnaire data - medical items
physactiv_raw <- read.xport("data/PAQ_I.XPT")    # Questionnaire data - physical activity
physfxn_raw <- read.xport("data/PFQ_I.XPT")      # Questionnaire data - physical functioning



#### Wrangling data ----



#### Sending data to .Rmd ----

# render("analysis_report_nb.Rmd")