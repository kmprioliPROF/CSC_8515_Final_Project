# Katherine M. Prioli
# CSC 8515 Final Project
# Sat Nov 02 10:31:33 2019 ------------------------------


#### Loading libraries ----

library(tidyverse)    # For data import and wrangling
library(haven)        # For loading SAS .xpt files
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

# Commenting the below - it may be easier not to use `haven`

# dietbehav_raw <- read_xpt("data/DBQ_I.XPT")       # Questionnaire data - dietary behavior
# dietintake_raw <- read_xpt("data/DR1TOT_I.XPT")   # Dietary intake data, Day 1
# PHQ9_raw <- read_xpt("data/DPQ_I.XPT")            # Questionnaire data - PHQ-9
# bloodpress_raw <- read_xpt("data/BPX_I.XPT")      # Examination data - blood pressure
# bodymeas_raw <- read_xpt("data/BMX_I.XPT")        # Examination data - body measures
# demog_raw <- read_xpt("data/DEMO_I.XPT")          # Demographics data
# diabetes_raw <- read_xpt("data/DIQ_I.XPT")        # Questionnaire data - diabetes items
# medical_raw <- read_xpt("data/MCQ_I.XPT")         # Questionnaire data - medical items
# physactiv_raw <- read_xpt("data/PAQ_I.XPT")       # Questionnaire data - physical activity
# physfxn_raw <- read_xpt("data/PFQ_I.XPT")         # Questionnaire data - physical functioning

dietbehav_raw <- read.xport("data/DBQ_I.XPT")       # Questionnaire data - dietary behavior
dietintake_raw <- read.xport("data/DR1TOT_I.XPT")   # Dietary intake data, Day 1
PHQ9_raw <- read.xport("data/DPQ_I.XPT")            # Questionnaire data - PHQ-9
bloodpress_raw <- read.xport("data/BPX_I.XPT")      # Examination data - blood pressure
bodymeas_raw <- read.xport("data/BMX_I.XPT")        # Examination data - body measures
demog_raw <- read.xport("data/DEMO_I.XPT")          # Demographics data
diabetes_raw <- read.xport("data/DIQ_I.XPT")        # Questionnaire data - diabetes items
medical_raw <- read.xport("data/MCQ_I.XPT")         # Questionnaire data - medical items
physactiv_raw <- read.xport("data/PAQ_I.XPT")       # Questionnaire data - physical activity
physfxn_raw <- read.xport("data/PFQ_I.XPT")         # Questionnaire data - physical functioning

#### Wrangling data ----

names(dietbehav_raw) <- str_to_lower(names(dietbehav_raw))

# Commenting the approach below - it may be easier not to use `haven`
# dietbehav <- dietbehav_raw %>% 
#   select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590) %>% 
#   mutate(dbq700 = factor(dbq700,
#                          levels = c(1, 2, 3, 4, 5, 7, 9), 
#                          labels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Refused", "Don't know")),
#          cbq505 = factor(cbq505,
#                          levels = c(1, 2, 7, 9),
#                          labels = c("Yes", "No", "Refused", "Don't know")),
#          cbq540 = factor(cbq540,
#                          levels = c(1, 2, 7, 9),
#                          labels = c("Yes", "No", "Refused", "Don't know")),
#          cbq545 = factor(cbq545,
#                          levels = c(1, 2, 3, 4, 7, 9),
#                          labels = c("Often", "Sometimes", "Rarely", "Never", "Refused", "Don't know")),
#          cbq550 = factor(cbq550,
#                          levels = c(1, 2, 7, 9),
#                          labels = c("Yes", "No", "Refused", "Don't know")),
#          cbq585 = factor(cbq585,
#                          levels = c(1, 2, 7, 9),
#                          labels = c("Yes", "No", "Refused", "Don't know")),
#          cbq590 = factor(cbq590,
#                          levels = c(1, 2, 3, 4, 7, 9),
#                          labels = c("Often", "Sometimes", "Rarely", "Never", "Refused", "Don't know")),
#          )

dietbehav_raw2 <- SASxport::read.xport("data/DBQ_I.XPT")
names(dietbehav_raw2) <- str_to_lower(names(dietbehav_raw2))
dietbehav2 <- dietbehav_raw2 %>% 
  select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590) %>% 
  #zap_empty() %>% 
  zap_missing()


# Removing unnecessary dataframes

rm(ls = c("dietbehav_raw", "dietintake_raw", "PHQ9_raw", "bloodpress_raw", "bodymeas_raw", "demog_raw",
          "diabetes_raw", "medical_raw", "physactiv_raw", "physfxn_raw"))


#### Sending data to .Rmd ----

# render("analysis_report_nb.Rmd")