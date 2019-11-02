# Katherine M. Prioli
# CSC 8515 Final Project
# Sat Nov 02 10:31:33 2019 ------------------------------


#### Loading libraries ----

library(tidyverse)    # For data import and wrangling
library(haven)        # For loading SAS .xpt files
library(forcats)      # For handling categorical data
library(gridExtra)    # For grid.arrange()
library(grid)         # For textGrob() to annotate grid.arrange() elements
library(kableExtra)   # For prettifying output tables
library(ggthemr)      # For prettifying output plots
library(rmarkdown)    # For `render()`

ggthemr("fresh")


#### Importing datasets ----

# NHANES 2015-2016 data (Source:  https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015)

dietbehav_raw <- read_xpt("data/DBQ_I.XPT")       # Questionnaire data - dietary behavior
dietintake_raw <- read_xpt("data/DR1TOT_I.XPT")   # Dietary intake data, Day 1
PHQ9_raw <- read_xpt("data/DPQ_I.XPT")            # Questionnaire data - PHQ-9
bloodpress_raw <- read_xpt("data/BPX_I.XPT")      # Examination data - blood pressure
bodymeas_raw <- read_xpt("data/BMX_I.XPT")        # Examination data - body measures
demog_raw <- read_xpt("data/DEMO_I.XPT")          # Demographics data
diabetes_raw <- read_xpt("data/DIQ_I.XPT")        # Questionnaire data - diabetes items
medical_raw <- read_xpt("data/MCQ_I.XPT")         # Questionnaire data - medical items
physactiv_raw <- read_xpt("data/PAQ_I.XPT")       # Questionnaire data - physical activity
physfxn_raw <- read_xpt("data/PFQ_I.XPT")         # Questionnaire data - physical functioning


#### Wrangling data ----

# Dietary behavior data

names(dietbehav_raw) <- str_to_lower(names(dietbehav_raw))

dietbehav_stag <- dietbehav_raw %>%
  select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590) %>%
  mutate(
    dbq700c = case_when(
      dbq700 > 5 ~ as.numeric(NA),
      TRUE ~ dbq700),
    dbq700c = factor(dbq700c,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    cbq505c = case_when(
      cbq505 > 3 ~ as.numeric(NA),
      TRUE ~ cbq505),
    cbq505c = factor(cbq505c,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    cbq540c = case_when(
      cbq540 > 3 ~ as.numeric(NA),
      TRUE ~ cbq540),
    cbq540c = factor(cbq540c,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    cbq545c = case_when(
      cbq545 > 4 ~ as.numeric(NA),
      TRUE ~ cbq545),
    cbq545c = factor(cbq545c,
                     levels = c(1, 2, 3, 4),
                     labels = c("Often", "Sometimes", "Rarely", "Never")),
    cbq550c = case_when(
      cbq550 > 3 ~ as.numeric(NA),
      TRUE ~ cbq550),
    cbq550c = factor(cbq550c,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    cbq585c = case_when(
      cbq585 > 3 ~ as.numeric(NA),
      TRUE ~ cbq585),
    cbq585c = factor(cbq585c,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    cbq590c = case_when(
      cbq590 > 4 ~ as.numeric(NA),
      TRUE ~ cbq590),
    cbq590c = factor(cbq590c,
                     levels = c(1, 2, 3, 4),
                     labels = c("Often", "Sometimes", "Rarely", "Never")))

dietbehav <- dietbehav_stag %>% select(-dbq700, -cbq505, -cbq540, -cbq545, -cbq550, -cbq585, -cbq590)

# Dietary intake data

names(dietintake_raw) <- str_to_lower(names(dietintake_raw))

dietintake_stag <- dietintake_raw %>% 
  select(seqn, dr1tkcal, dr1_300, dr1_320z) %>% 
  rename(dr1_32oz = dr1_320z) %>% 
  mutate(
    dr1_300c = case_when(
      dr1_300 > 3 ~ as.numeric(NA),
      TRUE ~ dr1_300),
    dr1_300c = factor(dr1_300c,
                      levels = c(1, 2, 3),
                      labels = c("Much more than usual",
                                 "Usual",
                                 "Much less than usual")))

dietintake <- dietintake_stag %>% select(-dr1_300)

# PHQ-9 data

names(PHQ9_raw) <- str_to_lower(names(PHQ9_raw))

PHQ9_stag <- PHQ9_raw %>% 
  select(-dpq100) %>% 
  mutate(
    dpq010c = case_when(
      dpq010 > 3 ~ as.numeric(NA),
      TRUE ~ dpq010),
    dpq020c = case_when(
      dpq020 > 3 ~ as.numeric(NA),
      TRUE ~ dpq020),
    dpq030c = case_when(
      dpq030 > 3 ~ as.numeric(NA),
      TRUE ~ dpq030),
    dpq040c = case_when(
      dpq040 > 3 ~ as.numeric(NA),
      TRUE ~ dpq040),
    dpq050c = case_when(
      dpq050 > 3 ~ as.numeric(NA),
      TRUE ~ dpq050),
    dpq060c = case_when(
      dpq060 > 3 ~ as.numeric(NA),
      TRUE ~ dpq060),
    dpq070c = case_when(
      dpq070 > 3 ~ as.numeric(NA),
      TRUE ~ dpq070),
    dpq080c = case_when(
      dpq080 > 3 ~ as.numeric(NA),
      TRUE ~ dpq080),
    dpq090c = case_when(
      dpq090 > 3 ~ as.numeric(NA),
      TRUE ~ dpq090)) %>% 
  mutate(phq9_score = rowSums(.[11:19], na.rm = FALSE),   # Ensuring only complete batteries are scored
         phq9_cat = case_when(
           phq9_score >=  0 & phq9_score <=  4 ~ 0,       # None to minimal
           phq9_score >=  5 & phq9_score <=  9 ~ 1,       # Mild
           phq9_score >= 10 & phq9_score <= 14 ~ 2,       # Moderate
           phq9_score >= 15 & phq9_score <= 19 ~ 3,       # Moderately severe
           phq9_score >= 20 & phq9_score <= 27 ~ 4,       # Severe
           TRUE ~ as.numeric(NA)),
         phq9_cat = factor(phq9_cat,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("None to minimal",
                                      "Mild",
                                      "Moderate",
                                      "Moderately severe",
                                      "Severe")))

PHQ9 <- PHQ9_stag %>% select(seqn, phq9_score, phq9_cat)

# Blood pressure data

names(bloodpress_raw) <- str_to_lower(names(bloodpress_raw))

bloodpress <- bloodpress_raw %>% 
  select(seqn, bpxsy2, bpxdi2) %>% 
  mutate(htn_cat = case_when(
    is.na(bpxsy2) == TRUE | is.na(bpxdi2) == TRUE ~ as.numeric(NA),
    bpxsy2 <  120 & bpxdi2 < 80 ~ 0,   # Normal BP
    bpxsy2 >= 120 & bpxdi2 < 80 ~ 1,   # Elevated BP
    (bpxsy2 >= 130 & bpxsy2 <= 139) | (bpxdi2 >= 80 & bpxdi2 <=  89) ~ 2,   # Stage 1 HTN
    (bpxsy2 >= 140 & bpxsy2 <= 180) | (bpxdi2 >= 90 & bpxdi2 <= 120) ~ 3,   # Stage 2 HTN
    bpxsy2 > 180 | bpxdi2 > 120 ~ 4   # Hypertensive crisis
  ),
  htn_cat = factor(htn_cat,
                   levels = c(0, 1, 2, 3, 4),
                   labels = c("Normal", "Elevated", "Stage 1 HTN", 
                              "Stage 2 HTN", "Hypertensive crisis")))

# Body measures data

names(bodymeas_raw) <- str_to_lower(names(bodymeas_raw))

bodymeas_stag <- bodymeas_raw %>% 
  select(seqn, bmxwt, bmxht, bmxbmi) %>% 
  mutate(BMI_cat = case_when(
    is.na(bmxbmi) == TRUE ~ as.numeric(NA),
    bmxbmi < 18.5 ~ 1,                    # Underweight
    bmxbmi >= 18.5 & bmxbmi < 25.0 ~ 2,   # Normal weight
    bmxbmi >= 25.0 & bmxbmi < 30.0 ~ 3,   # Overweight
    bmxbmi >= 30.0 ~ 4                    # Obese
  ))
  
bodymeas <- bodymeas_stag %>% select(-bmxwt, -bmxwt)

# Removing raw dataframes

rm(ls = c("dietbehav_raw", "dietintake_raw", "PHQ9_raw", "bloodpress_raw", "bodymeas_raw", "demog_raw",
          "diabetes_raw", "medical_raw", "physactiv_raw", "physfxn_raw"))


#### Sending data to .Rmd ----

# render("analysis_report_nb.Rmd")