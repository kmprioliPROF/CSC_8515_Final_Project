# Katherine M. Prioli
# CSC 8515 Final Project - import & wrangle data
# Sun Nov 24 23:33:04 2019 ------------------------------


#### Loading libraries ----

library(here)           # For easy sourcing from project directory
library(haven)          # For loading SAS .xpt files
library(sjlabelled)     # For removing pesky column labels
library(forcats)        # For handling categorical data
library(psych)          # For describe()
library(randomForest)   # For rfImpute()
library(vegan)          # For vegdist() (computing Gower distance)
library(cluster)        # For diana()
library(fpc)            # For cqcluster.stats()
library(ggdendro)       # For ggdendrogram()
library(dendextend)     # For coloring dendrogram labels by BMI_cat
library(gridExtra)      # For grid.arrange()
library(grid)           # For textGrob() to annotate grid.arrange() elements
library(rmarkdown)      # For render()
library(reticulate)     # For interfacing with Python in .Rmd
library(kableExtra)     # For prettifying output tables
library(broom)          # For tidy()
library(ggthemr)        # For prettifying output plots
library(tidyselect)     # For selecting by string
library(tidyverse)      # For data import and wrangling

ggthemr("flat")


#### Importing datasets ----

# NHANES 2015-2016 data ( Source:  https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015 )

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

names(dietbehav_raw) <- str_to_lower(names(dietbehav_raw))
names(dietintake_raw) <- str_to_lower(names(dietintake_raw))
names(PHQ9_raw) <- str_to_lower(names(PHQ9_raw))
names(bloodpress_raw) <- str_to_lower(names(bloodpress_raw))
names(bodymeas_raw) <- str_to_lower(names(bodymeas_raw))
names(demog_raw) <- str_to_lower(names(demog_raw))
names(diabetes_raw) <- str_to_lower(names(diabetes_raw))
names(medical_raw) <- str_to_lower(names(medical_raw))
names(physactiv_raw) <- str_to_lower(names(physactiv_raw))
names(physfxn_raw) <- str_to_lower(names(physfxn_raw))

# Generating list of unique identifiers

a <- bloodpress_raw$seqn %>% as_tibble()
b <- bodymeas_raw$seqn %>% as_tibble()
c <- demog_raw$seqn %>% as_tibble()
d <- diabetes_raw$seqn %>% as_tibble()
e <- dietbehav_raw$seqn %>% as_tibble()
f <- dietintake_raw$seqn %>% as_tibble()
g <- medical_raw$seqn %>% as_tibble()
h <- PHQ9_raw$seqn %>% as_tibble()
i <- physactiv_raw$seqn %>% as_tibble()
j <- physfxn_raw$seqn %>% as_tibble()

allseqn <- rbind(a, b, c, d, e, f, g, h, i, j) %>% 
  unique() %>% 
  arrange() %>% 
  rename(seqn = value)

rm(list = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))


# Joining datasets

demog_stag <- demog_raw %>% 
  select(seqn, riagendr, ridageyr, ridreth3, dmdeduc2, dmdmartl, indfmin2, indfmpir)

diabetes_stag <- diabetes_raw %>% 
  select(seqn, diq010, diq280)

medical_stag <- medical_raw %>% 
  select(seqn, mcq160c, mcq160e, mcq160m, mcq365a, mcq365b)

bloodpress_stag <- bloodpress_raw %>% 
  select(seqn, bpxsy2, bpxdi2)

physactiv_stag <- physactiv_raw %>% 
  select(seqn, pad615, pad630, pad660, pad675, pad680)

physfxn_stag <- physfxn_raw %>% 
  select(seqn, pfq049, pfq061b)

PHQ9_stag <- PHQ9_raw %>% 
  select(-dpq100)

dietbehav_stag <- dietbehav_raw %>%
  select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590)

dietintake_stag <- dietintake_raw %>% 
  select(seqn, dr1tkcal, dr1_300, dr1_320z)

bodymeas_stag <- bodymeas_raw %>% 
  select(seqn, bmxbmi)

nhanes_stag <- allseqn %>%   # These joins will throw a warning; ignore it - this is a known dplyr issue
  left_join(demog_stag, by = c("seqn" = "seqn")) %>%
  left_join(diabetes_stag, by = c("seqn" = "seqn")) %>%
  left_join(medical_stag, by = c("seqn" = "seqn")) %>%
  left_join(bloodpress_stag, by = c("seqn" = "seqn")) %>%
  left_join(physactiv_stag, by = c("seqn" = "seqn")) %>%
  left_join(physfxn_stag, by = c("seqn" = "seqn")) %>%
  left_join(PHQ9_stag, by = c("seqn" = "seqn")) %>%
  left_join(dietbehav_stag, by = c("seqn" = "seqn")) %>%
  left_join(dietintake_stag, by = c("seqn" = "seqn")) %>%
  left_join(bodymeas_stag, by = c("seqn" = "seqn")) %>%
  remove_all_labels() %>%    # Removing annoying column labels
  
  # Questionnaire data - dietary behavior
  
  mutate(
    diethealthy = case_when(
      dbq700 > 0 & dbq700 < 6 ~ dbq700,
      TRUE ~ 6),
    diethealthy_fct = factor(diethealthy, ordered = TRUE,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Missing")),
    fastfood_eat = case_when(
      cbq505 > 0 & cbq505 < 3 ~ cbq505,
      TRUE ~ 3),
    fastfood_eat_fct = factor(fastfood_eat, ordered = FALSE,
                              levels = c(1, 2, 3),
                              labels = c("Yes", "No", "Missing")),
    fastfood_usednutrit = case_when(
      cbq540 > 0 & cbq540 < 3 ~ cbq540,
      TRUE ~ 3),
    fastfood_usednutrit_fct = factor(fastfood_usednutrit, ordered = FALSE,
                                     levels = c(1, 2, 3),
                                     labels = c("Yes", "No", "Missing")),
    fastfood_woulduse = case_when(
      cbq545 > 0 & cbq545 < 5 ~ cbq545,
      TRUE ~ 5),
    fastfood_woulduse_fct = factor(fastfood_woulduse, ordered = TRUE,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("Often", "Sometimes", "Rarely", "Never", "Missing")),
    restaur_eat = case_when(
      cbq550 > 0 & cbq550 < 3 ~ cbq550,
      TRUE ~ 3),
    restaur_eat_fct = factor(restaur_eat, ordered = FALSE,
                             levels = c(1, 2, 3),
                             labels = c("Yes", "No", "Missing")),
    restaur_usednutrit = case_when(
      cbq585 > 0 & cbq585 < 3 ~ cbq585,
      TRUE ~ 3),
    restaur_usednutrit_fct = factor(restaur_usednutrit, ordered = FALSE,
                                    levels = c(1, 2, 3),
                                    labels = c("Yes", "No", "Missing")),
    restaur_woulduse = case_when(
      cbq590 > 0 & cbq590 < 5 ~ cbq590,
      TRUE ~ 5),
    restaur_woulduse_fct = factor(restaur_woulduse, ordered = TRUE,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Often", "Sometimes", "Rarely", "Never", "Missing"))) %>% 
  select(-dbq700, -cbq505, -cbq540, -cbq545, -cbq550, -cbq585, -cbq590) %>% 
  
  # Dietary intake data, Day 1
  
  rename(dailykcal = dr1tkcal,
         dailywater = dr1_320z) %>% 
  mutate(
    dailykcal_typical = case_when(
      dr1_300 > 0 & dr1_300 < 4 ~ dr1_300,
      TRUE ~ 4),
    dailykcal_typical_fct = factor(dailykcal_typical, ordered = TRUE,
                                   levels = c(1, 2, 3, 4),
                                   labels = c("Much more than usual",
                                              "Usual",
                                              "Much less than usual",
                                              "Missing"))) %>% 
  select(-dr1_300) %>% 
  
  # Questionnaire data - PHQ-9
  
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
  select(-dpq010, -dpq020, -dpq030, -dpq040, -dpq050, -dpq060, -dpq070, -dpq080, -dpq090, -dpq010) %>% 
  mutate(PHQ9_score = rowSums(.[grep("dpq0", names(.))], na.rm = FALSE),   # Ensuring only complete batteries are scored
         PHQ9_cat = case_when(
           PHQ9_score >=  0 & PHQ9_score <=  4 ~ 0,       # None to minimal
           PHQ9_score >=  5 & PHQ9_score <=  9 ~ 1,       # Mild
           PHQ9_score >= 10 & PHQ9_score <= 14 ~ 2,       # Moderate
           PHQ9_score >= 15 & PHQ9_score <= 19 ~ 3,       # Moderately severe
           PHQ9_score >= 20 & PHQ9_score <= 27 ~ 4,       # Severe
           TRUE ~ 5),
         PHQ9_cat_fct = factor(PHQ9_cat, ordered = TRUE,
                               levels = c(0, 1, 2, 3, 4, 5),
                               labels = c("None to minimal",
                                          "Mild",
                                          "Moderate",
                                          "Moderately severe",
                                          "Severe",
                                          "Missing"))) %>% 
  select(-dpq010c, -dpq020c, -dpq030c, -dpq040c, -dpq050c, -dpq060c, -dpq070c, -dpq080c, -dpq090c, -dpq010c) %>% 
  
  # Examination data - blood pressure
  
  rename(systolic = bpxsy2,
         diastolic = bpxdi2) %>% 
  mutate(BP_cat = case_when(
    systolic < 90 | diastolic < 60 ~ 0,                                               # Hypotension
    systolic >= 90 & systolic < 120 & diastolic >= 60 & diastolic < 80 ~ 1,           # Normal BP
    systolic >= 120 & diastolic < 80 ~ 2,                                             # Elevated BP
    (systolic >= 130 & systolic <= 139) | (diastolic >= 80 & diastolic <=  89) ~ 3,   # Stage 1 HTN
    (systolic >= 140 & systolic <= 180) | (diastolic >= 90 & diastolic <= 120) ~ 4,   # Stage 2 HTN
    systolic > 180 | diastolic > 120 ~ 5,                                             # Hypertensive crisis
    is.na(systolic) == TRUE | is.na(diastolic) == TRUE ~ 6),
    BP_cat_fct = factor(BP_cat, ordered = TRUE,
                        levels = c(0, 1, 2, 3, 4, 5, 6),
                        labels = c("Hypotensive", "Normal", "Elevated", "Stage 1 HTN", "Stage 2 HTN", "Hypertensive crisis", "Missing"))) %>% 
  
  # Examination data - body measures
  
  rename(BMI = bmxbmi) %>% 
  mutate(
    BMI_cat = case_when(
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 25.0 ~ 2,
      BMI >= 25.0 & BMI < 30.0 ~ 3,
      BMI >= 30.0 ~ 4,
      is.na(BMI) == TRUE ~ 5),
    BMI_cat_fct = factor(BMI_cat,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Underweight", "Normal weight", "Overweight", "Obese", "Missing"))) %>% 
  
  # Demographics data
  
  rename(gender = riagendr,
         age = ridageyr,
         race = ridreth3,
         famincome_povratio = indfmpir) %>% 
  mutate(gender_fct = factor(gender, ordered = FALSE,
                             levels = c(1, 2),
                             labels = c("Male", "Female")),
         race = case_when(
           race == 1 | race == 2 ~ 1,
           TRUE ~ race),
         race_fct = factor(race, ordered = FALSE,
                           levels = c(1, 3, 4, 6, 7),
                           labels = c("Hispanic",
                                      "Non-Hispanic white",
                                      "Non-Hispanic black",
                                      "Non-Hispanic Asian",
                                      "Other or mixed race")),
         educ = case_when(
           dmdeduc2 > 0 & dmdeduc2 < 6 ~ dmdeduc2,
           TRUE ~ 6),
         educ_fct = factor(educ, ordered = TRUE,
                           levels = c(1, 2, 3, 4, 5, 6),
                           labels = c("Less than 9th grade",
                                      "9th-12th grade, no HS diploma",
                                      "High school / GED",
                                      "Some college / AA degree",
                                      "College graduate or above",
                                      "Missing")),
         marital = case_when(
           dmdmartl > 0 & dmdmartl < 7 ~ dmdmartl,
           TRUE ~ 7),
         marital_fct = factor(marital, ordered = FALSE,
                              levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("Married",
                                         "Widowed",
                                         "Divorced",
                                         "Separated",
                                         "Never married",
                                         "Living with partner",
                                         "Missing")),
         famincome_cat = case_when(
           is.na(indfmin2) == TRUE ~ 16,
           (indfmin2 >= 11 & indfmin2 <= 13) | indfmin2 > 15 ~ 16,
           TRUE ~ indfmin2),
         famincome_cat_fct = factor(famincome_cat, ordered = TRUE,
                                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16),
                                    labels = c("$0 to $4,999",       # 1
                                               "$5,000 to $9,999",   # 2
                                               "$10,000 to $14,999", # 3
                                               "$15,000 to $19,999", # 4
                                               "$20,000 to $24,999", # 5
                                               "$25,000 to $34,999", # 6
                                               "$35,000 to $44,999", # 7
                                               "$45,000 to $54,999", # 8
                                               "$55,000 to $64,999", # 9
                                               "$65,000 to $74,999", # 10
                                               "$75,000 to $99,999", # 14
                                               "$100,000 and over",  # 15
                                               "Missing"))) %>%      # 16 (comprises 12, 13, 77, 99, and missing)
  select(-dmdeduc2, -dmdmartl, -indfmin2) %>% 
  
  # Questionnaire data - diabetes items
  
  mutate(
    diabet_hx = case_when(
      diq010 > 0 & diq010 < 4 ~ diq010,
      TRUE ~ 4),
    diabet_hx_fct = factor(diabet_hx, ordered = FALSE,
                           levels = c(1, 2, 3, 4),
                           labels = c("Yes", "No", "Borderline", "Missing")),
    hba1c = case_when(
      diq280 > 18.5 ~ as.numeric(NA),
      TRUE ~ diq280)) %>% 
  select(-diq010, -diq280) %>% 
  
  # Questionnaire data - medical items
  
  mutate(
    CAD_hx = case_when(
      mcq160c > 0 & mcq160c < 3 ~ mcq160c,
      TRUE ~ 3),
    CAD_hx_fct = factor(CAD_hx, ordered = FALSE,
                        levels = c(1, 2, 3),
                        labels = c("Yes", "No", "Missing")),
    MI_hx = case_when(
      mcq160e > 0 & mcq160e < 3 ~ mcq160e,
      TRUE ~ 3),
    MI_hx_fct = factor(MI_hx, ordered = FALSE,
                       levels = c(1, 2, 3),
                       labels = c("Yes", "No", "Missing")),
    thy_hx = case_when(
      mcq160m > 0 & mcq160m < 3 ~ mcq160m,
      TRUE ~ 3),
    thy_hx_fct = factor(thy_hx, ordered = FALSE,
                        levels = c(1, 2, 3),
                        labels = c("Yes", "No", "Missing")),
    doc_losewt = case_when(
      mcq365a > 0 & mcq365a < 3 ~ mcq365a,
      TRUE ~ 3),
    doc_losewt_fct = factor(doc_losewt, ordered = FALSE,
                            levels = c(1, 2, 3),
                            labels = c("Yes", "No", "Missing")),
    doc_exer = case_when(
      mcq365b > 0 & mcq365b < 3 ~ mcq365b,
      TRUE ~ 3),
    doc_exer_fct = factor(doc_exer, ordered = FALSE,
                          levels = c(1, 2, 3),
                          labels = c("Yes", "No", "Missing"))) %>% 
  select(-mcq160c, -mcq160e, -mcq160m, -mcq365a, -mcq365b) %>% 
  
  # Questionnaire data - physical activity
  
  mutate(
    mins_vigwork = case_when(
      pad615 > 840 ~ as.numeric(NA),
      TRUE ~ pad615),
    mins_modwork = case_when(
      pad630 > 990 ~ as.numeric(NA),
      TRUE ~ pad630),
    mins_vigrec = case_when(
      pad660 > 480 ~ as.numeric(NA),
      TRUE ~ pad660),
    mins_modrec = case_when(
      pad675 > 600 ~ as.numeric(NA),
      TRUE ~ pad675),
    mins_seden = case_when(
      pad680 > 1380 ~ as.numeric(NA),
      TRUE ~ pad680)) %>% 
  rowwise() %>%
  mutate(mins_activ = sum(mins_vigwork, mins_modwork, mins_vigrec, mins_modrec, na.rm = TRUE)) %>% 
  ungroup() %>%   # Undoing grouping induced by `rowwise()`
  select(-pad615, -pad630, -pad660, -pad675, -pad680, -mins_vigwork, -mins_modwork, -mins_vigrec, -mins_modrec) %>% 
  
  # Questionnaire data - physical functioning
  
  mutate(
    worklim = case_when(
      pfq049 > 0 & pfq049 < 3 ~ pfq049,
      TRUE ~ 3),
    worklim_fct = factor(worklim, ordered = FALSE,
                         levels = c(1, 2, 3),
                         labels = c("Yes", "No", "Missing")),
    walklim = case_when(
      pfq061b > 0 & pfq061b < 6 ~ pfq061b,
      TRUE ~ 6),
    walklim_fct = factor(walklim, ordered = TRUE,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("No difficulty", "Some difficulty", "Much difficulty", "Unable to do", 
                                    "Do not do this activity", "Missing"))) %>% 
  mutate(
    comorbid_diabet = case_when(
      diabet_hx == 1 | diabet_hx == 3 ~ 1,
      TRUE ~ 0),
    comorbid_thy = case_when(
      thy_hx == 1 ~ 1,
      TRUE ~ 0),
    comorbid_MI = case_when(
      MI_hx == 1 ~ 1,
      TRUE ~ 0),
    comorbid_CAD = case_when(
      CAD_hx == 1 ~ 1,
      TRUE ~ 0),
    comorbid_HTN = case_when(
      BP_cat >= 3 ~ 1,
      TRUE ~ 0),
    comorbid_depr = case_when(
      PHQ9_cat >= 1 & PHQ9_cat <=4 ~ 1,
      TRUE ~ 0)) %>% 
  mutate(n_comorbid = comorbid_diabet + comorbid_thy + comorbid_MI + comorbid_CAD + comorbid_HTN + comorbid_depr) %>% 
  filter(age >= 20 & !is.na(BMI) == TRUE) %>%   # Ensuring all cases are adult subjects with known BMI
  select(seqn, age, gender, gender_fct, race, race_fct, educ, educ_fct, marital, marital_fct, famincome_cat, famincome_cat_fct, famincome_povratio,
         diabet_hx, diabet_hx_fct, hba1c, CAD_hx, CAD_hx_fct, MI_hx, MI_hx_fct, thy_hx, thy_hx_fct, 
         doc_losewt, doc_losewt_fct, doc_exer, doc_exer_fct,
         systolic, diastolic, BP_cat, BP_cat_fct,
         n_comorbid,
         mins_activ, mins_seden, worklim, worklim_fct, walklim, walklim_fct,
         diethealthy, diethealthy_fct, fastfood_eat, fastfood_eat_fct, fastfood_usednutrit, fastfood_usednutrit_fct, 
         fastfood_woulduse, fastfood_woulduse_fct, restaur_eat, restaur_eat_fct, restaur_usednutrit, restaur_usednutrit_fct, 
         restaur_woulduse, restaur_woulduse_fct,
         dailykcal, dailykcal_typical, dailykcal_typical_fct, dailywater,
         PHQ9_score, PHQ9_cat, PHQ9_cat_fct,
         BMI, BMI_cat, BMI_cat_fct)

# Removing raw and staging dataframes

rm(list = c("dietbehav_raw", "dietbehav_stag",
            "dietintake_raw", "dietintake_stag",
            "PHQ9_raw", "PHQ9_stag",
            "bloodpress_raw", "bloodpress_stag",
            "bodymeas_raw", "bodymeas_stag",
            "demog_raw", "demog_stag",
            "diabetes_raw", "diabetes_stag",
            "medical_raw", "medical_stag",
            "physactiv_raw", "physactiv_stag",
            "physfxn_raw", "physfxn_stag"))


#### Exploratory analysis ----

# Continuous variables

nhanes_contin <- nhanes_stag %>%
  select(age, famincome_povratio, hba1c, systolic, diastolic, mins_activ, mins_seden, dailykcal, dailywater, PHQ9_score, n_comorbid, BMI)

nhanes_contin_desc <- nhanes_contin %>% 
  describe()

contins <- rownames(nhanes_contin_desc)

nhanes_contin_kable <- nhanes_contin_desc %>% 
  mutate(vars = contins) %>% 
  select(vars, n, mean, sd, min, max, median) %>% 
  mutate(mean = round(mean, digits = 2),
         sd = round(sd, digits = 3)) %>% 
  kable(format = "markdown")

nhanes_fct <- nhanes_stag %>%
  select(gender_fct, race_fct, educ_fct, marital_fct, famincome_cat_fct, diabet_hx_fct, CAD_hx_fct, 
         MI_hx_fct, thy_hx_fct, doc_losewt_fct, doc_exer_fct, BP_cat_fct, worklim_fct, walklim_fct, 
         diethealthy_fct, fastfood_eat_fct, fastfood_usednutrit_fct, fastfood_woulduse_fct, restaur_eat_fct, 
         restaur_usednutrit_fct, restaur_woulduse_fct, dailykcal_typical_fct, PHQ9_cat_fct, BMI_cat_fct)

categsumm <- function(df, x){
  summdf <- df %>%
    select(!!x) %>%
    mutate(lev = as.numeric(!!x)) %>% 
    group_by(!!x) %>%
    arrange(lev) %>% 
    summarize(n = n()) %>%
    mutate(pct = round((n / sum(n)) * 100, digits = 2))
  dfout <- return(summdf) %>% kable(format = "markdown")
}

# For loop isn't working; not worth spending the time to fix it

# for(i in 1:length(nhanes_categvars)){
#   assign(paste0("summ_", nhanes_categvars[[i]]), categsumm(nhanes, quo(nhanes_categvars[[i]])))
# }

gender_summ <- categsumm(nhanes_fct, quo(gender_fct))
race_summ <- categsumm(nhanes_fct, quo(race_fct))
educ_summ <- categsumm(nhanes_fct, quo(educ_fct))
marital_summ <- categsumm(nhanes_fct, quo(marital_fct))
famincome_cat_summ <- categsumm(nhanes_fct, quo(famincome_cat_fct))
diabet_hx_summ <- categsumm(nhanes_fct, quo(diabet_hx_fct))
CAD_hx_summ <- categsumm(nhanes_fct, quo(CAD_hx_fct))
MI_hx_summ <- categsumm(nhanes_fct, quo(MI_hx_fct))
thy_hx_summ <- categsumm(nhanes_fct, quo(thy_hx_fct))
doc_losewt_summ <- categsumm(nhanes_fct, quo(doc_losewt_fct))
doc_exer_summ <- categsumm(nhanes_fct, quo(doc_exer_fct))
BP_cat_summ <- categsumm(nhanes_fct, quo(BP_cat_fct))
worklim_summ <- categsumm(nhanes_fct, quo(worklim_fct))
walklim_summ <- categsumm(nhanes_fct, quo(walklim_fct))
diethealthy_summ <- categsumm(nhanes_fct, quo(diethealthy_fct))
fastfood_eat_summ <- categsumm(nhanes_fct, quo(fastfood_eat_fct))
fastfood_usednutrit_summ <- categsumm(nhanes_fct, quo(fastfood_usednutrit_fct))
fastfood_woulduse_summ <- categsumm(nhanes_fct, quo(fastfood_woulduse_fct))
restaur_eat_summ <- categsumm(nhanes_fct, quo(restaur_eat_fct))
restaur_usednutrit_summ <- categsumm(nhanes_fct, quo(restaur_usednutrit_fct))
restaur_woulduse_summ <- categsumm(nhanes_fct, quo(restaur_woulduse_fct))
dailykcal_typical_summ <- categsumm(nhanes_fct, quo(dailykcal_typical_fct))
PHQ9_cat_summ <- categsumm(nhanes_fct, quo(PHQ9_cat_fct))
BMI_cat_summ <- categsumm(nhanes_fct, quo(BMI_cat_fct))


#### Omitting variables not needed ----

nhanes_stag2 <- nhanes_stag %>%
  mutate(losewt_exer = case_when(
    doc_losewt == 1 & doc_exer == 1 ~ 2,   # Both lose weight & exercise (expect 1367)
    (doc_losewt == 1 & doc_exer == 2) | (doc_losewt == 2 & doc_exer == 1) ~ 1,   # Either lose weight or exercise (expect 1053)
    doc_losewt == 2 & doc_exer == 2 ~ 0,   # Neither lose weight nor exercise (expect 2984)
    TRUE ~ 3)) %>%   # Missing (expect 2)
  select(-BMI, -hba1c, -systolic, -diastolic, -diabet_hx, -diabet_hx_fct, -CAD_hx, -CAD_hx_fct, -MI_hx, 
    -MI_hx_fct, -thy_hx, -thy_hx_fct, -doc_losewt, -doc_losewt_fct, -doc_exer, -doc_exer_fct, -BP_cat, 
    -BP_cat_fct, -dailykcal_typical_fct, -PHQ9_cat, -PHQ9_cat_fct) %>% 
  select(1:38, 41, 40, 39)   # Reordering such that BMI_cat is last listed


#### Imputing missing values for continuous data ----

set.seed(20191205)   # Setting seed to ensure stable results
nhanes_imputed <- rfImpute(BMI_cat ~ . -seqn, nhanes_stag2)   # Ensuring seqn doesn't get used for imputation
set.seed(NULL)

# Looking at descriptives for `nhanes_stag2` vs `nhanes_imputed`

nhanes_contin2 <- nhanes_stag2 %>%
  select(age, famincome_povratio, mins_activ, mins_seden, dailykcal, dailywater, PHQ9_score, n_comorbid)

nhanes_contin2_desc <- nhanes_contin2 %>% 
  describe()

contins2 <- rownames(nhanes_contin2_desc)

nhanes_contin2_kable <- nhanes_contin2_desc %>% 
  mutate(vars = contins2) %>% 
  select(vars, n, mean, sd, min, max, median) %>% 
  mutate(mean = round(mean, digits = 2),
         sd = round(sd, digits = 3)) %>% 
  kable(format = "markdown")

nhanes_contin_imp <- nhanes_imputed %>%
  select(age, famincome_povratio, mins_activ, mins_seden, dailykcal, dailywater, PHQ9_score, n_comorbid)

nhanes_contin_imp_desc <- nhanes_contin_imp %>% 
  describe()

contins_imp <- rownames(nhanes_contin_imp_desc)

nhanes_contin_imp_kable <- nhanes_contin_imp_desc %>% 
  mutate(vars = contins_imp) %>% 
  select(vars, n, mean, sd, min, max, median) %>% 
  mutate(mean = round(mean, digits = 2),
         sd = round(sd, digits = 3)) %>% 
  kable(format = "markdown")

nhanes_contin2_kable
nhanes_contin_imp_kable

# Comparing descriptives from `nhanes_stag2` to `nhanes_imputed`

# famincome_povratio_wilcox <- wilcox.test(nhanes_stag2$famincome_povratio, nhanes_imputed$famincome_povratio, 
#                                          alternative = "two.sided", 
#                                          conf.int = TRUE)

# PHQ9_score_wilcox <- wilcox.test(nhanes_stag2$PHQ9_score, nhanes_imputed$PHQ9_score,
#                                  alternative = "two.sided",
#                                  conf.int = TRUE)

age_wilcox <- wilcox.test(nhanes_stag2$age, nhanes_imputed$age, 
                          alternative = "two.sided", 
                          conf.int = TRUE)

mins_activ_wilcox <- wilcox.test(nhanes_stag2$mins_activ, nhanes_imputed$mins_activ,
                                 alternative = "two.sided", 
                                 conf.int = TRUE)

mins_seden_wilcox <- wilcox.test(nhanes_stag2$mins_seden, nhanes_imputed$mins_seden,
                                 alternative = "two.sided", 
                                 conf.int = TRUE)

dailykcal_wilcox <- wilcox.test(nhanes_stag2$dailykcal, nhanes_imputed$dailykcal,
                                alternative = "two.sided",
                                conf.int = TRUE)

dailywater_wilcox <- wilcox.test(nhanes_stag2$dailywater, nhanes_imputed$dailywater,
                                 alternative = "two.sided",
                                 conf.int = TRUE)

wilcox_vars <- c(#"PHQ9_score",
                 #"famincome_povratio", 
                 "age", 
                 "mins_activ", 
                 "mins_seden", 
                 "dailykcal", 
                 "dailywater") %>% 
  as_tibble() %>% 
  rename(variable = value)

wilcox_pvals <- c(#famincome_povratio_wilcox$p.value,
                  #PHQ9_score_wilcox$p.value,
                  age_wilcox$p.value,
                  mins_activ_wilcox$p.value,
                  mins_seden_wilcox$p.value,
                  dailykcal_wilcox$p.value,
                  dailywater_wilcox$p.value) %>% 
  as_tibble() %>% 
  rename(p_value = value)

wilcox_results <- cbind(wilcox_vars, wilcox_pvals) %>% as_tibble()

omits <- wilcox_results %>% 
  filter(p_value < 0.05) %>% 
  select(-p_value) %>% 
  as.list()


#### Finalizing analytic datasets ----

nhanes <- nhanes_imputed %>% 
  select(-one_of(!!quo(omits$variable)), -famincome_povratio, -PHQ9_score)

nhanes_dichot <- nhanes %>%   # Creating a dichotomous BMI variable bucketing under/normal with over/obese
  mutate(BMI_dichot = case_when(
    BMI_cat >= 1 & BMI_cat <= 2 ~ 1,
    BMI_cat >= 3 & BMI_cat <=4 ~ 2
  )) %>% 
  select(-BMI_cat) %>% 
  rename(BMI_cat = BMI_dichot)

# Creating smaller datasets for supervised approach that omit some variables which are largely NA or may not be useful

nhanes_sup_trim <- nhanes %>%
  select(age, gender, race, educ, marital, famincome_cat, n_comorbid, mins_activ, mins_seden, fastfood_eat, 
         restaur_eat, dailykcal, dailywater, losewt_exer, BMI_cat)

nhanes_sup_trim_dichot <- nhanes_dichot %>%
  select(age, gender, race, educ, marital, famincome_cat, n_comorbid, mins_activ, mins_seden, fastfood_eat, 
         restaur_eat, dailykcal, dailywater, losewt_exer, BMI_cat)

# Creating final unsupervised approach dataset (no factors, no NAs)

nhanes_unsup_stag <- nhanes %>% 
  select(seqn, age, gender, race, educ, marital, famincome_cat, n_comorbid, mins_activ,
         mins_seden, worklim, walklim, diethealthy, fastfood_eat, fastfood_usednutrit,
         fastfood_woulduse, restaur_eat, restaur_usednutrit, restaur_woulduse, dailykcal,
         dailywater, losewt_exer, BMI_cat) %>% 
  mutate(
    educ = case_when(
      educ != 6 ~ educ,
      TRUE ~ as.numeric(NA)),
    marital = case_when(
      marital != 7 ~ marital,
      TRUE ~ as.numeric(NA)),
    famincome_cat = case_when(
      famincome_cat != 16 ~ famincome_cat,
      TRUE ~ as.numeric(NA)),
    worklim = case_when(
      worklim != 3 ~ worklim,
      TRUE ~ as.numeric(NA)),
    walklim = case_when(
      walklim != 6 ~ walklim,
      TRUE ~ as.numeric(NA)),
    diethealthy = case_when(
      diethealthy != 6 ~ diethealthy,
      TRUE ~ as.numeric(NA)),
    fastfood_eat = case_when(
      fastfood_eat != 3 ~ fastfood_eat,
      TRUE ~ as.numeric(NA)),
    fastfood_usednutrit = case_when(
      fastfood_usednutrit != 3 ~ fastfood_usednutrit,
      TRUE ~ as.numeric(NA)),
    fastfood_woulduse = case_when(
      fastfood_woulduse != 5 ~ fastfood_woulduse,
      TRUE ~ as.numeric(NA)),
    restaur_eat = case_when(
      restaur_eat != 3 ~ restaur_eat,
      TRUE ~ as.numeric(NA)),
    restaur_usednutrit = case_when(
      restaur_usednutrit != 3 ~ restaur_usednutrit,
      TRUE ~ as.numeric(NA)),
    restaur_woulduse = case_when(
      restaur_woulduse != 5 ~ restaur_woulduse,
      TRUE ~ as.numeric(NA)),
    losewt_exer = case_when(
      losewt_exer != 3 ~ losewt_exer,
      TRUE ~ as.numeric(NA)))

nhanes_unsup_full <- nhanes_unsup_stag %>% 
  select(-c("walklim", "fastfood_usednutrit", "fastfood_woulduse",   # Removing cols that are <90% complete
            "restaur_usednutrit", "restaur_woulduse")) %>% 
  drop_na()   # Removing cases with incomplete data

nhanes_unsup_stag2 <- nhanes_unsup_full %>% 
  group_by(BMI_cat) %>% 
  sample_frac(0.05) %>%   # Creating stratified 5% sample for use in unsupervised analysis
  ungroup()

unsup_labs <- nhanes_unsup_stag2 %>% 
  select(BMI_cat, seqn) %>% 
  mutate(rnames = paste0(BMI_cat, "_", seqn)) %>% 
  select(rnames) %>% 
  as_vector()

unsup_seqn <- nhanes_unsup_stag2$seqn   # Just in case I need this later

nhanes_unsup_mat <- nhanes_unsup_stag2 %>% 
  select(-seqn, BMI_cat) %>% 
  as.matrix()

rownames(nhanes_unsup_mat) <- unsup_labs


#### Removing staging and other unnecessary dataframes ----

rm(list = c("allseqn", "nhanes_stag", "nhanes_stag2", "nhanes_contin", "nhanes_contin2", "nhanes_contin_desc",
            "nhanes_contin2_desc", "nhanes_contin_imp", "nhanes_contin_imp_desc", "nhanes_fct", "nhanes_imputed",
            "omits", "wilcox_pvals", "wilcox_results", "wilcox_vars", "nhanes_unsup_stag"))


#### Rendering .Rmd ----
# render("checkpoints/Prioli_checkpoint1.Rmd")
# render("checkpoints/Prioli_checkpoint2.Rmd")
# render("checkpoints/Prioli_checkpoint3.Rmd")
# render("Prioli_final_report.Rmd")