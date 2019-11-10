# Katherine M. Prioli
# CSC 8515 Final Project
# Sun Nov 10 15:16:30 2019 ------------------------------


#### Loading libraries ----

library(tidyverse)    # For data import and wrangling
library(haven)        # For loading SAS .xpt files
library(sjlabelled)   # For removing pesky column labels
library(forcats)      # For handling categorical data
library(psych)        # For describe()
library(skimr)        # For skim()
library(gridExtra)    # For grid.arrange()
library(grid)         # For textGrob() to annotate grid.arrange() elements
library(rmarkdown)    # For render()
library(kableExtra)   # For prettifying output tables
library(ggthemr)      # For prettifying output plots

ggthemr("fresh")


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

# Questionnaire data - dietary behavior

names(dietbehav_raw) <- str_to_lower(names(dietbehav_raw))

dietbehav_stag <- dietbehav_raw %>%
  select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590) %>%
  mutate(
    diethealthy = case_when(
      dbq700 > 0 & dbq700 < 6 ~ dbq700,
      TRUE ~ 6),
    diethealthy_fct = factor(diethealthy,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Missing")),
    fastfood_eat = case_when(
      cbq505 > 0 & cbq505 < 3 ~ cbq505,
      TRUE ~ 3),
    fastfood_eat_fct = factor(fastfood_eat,
                          levels = c(1, 2, 3),
                          labels = c("Yes", "No", "Missing")),
    fastfood_usednutrit = case_when(
      cbq540 > 0 & cbq540 < 3 ~ cbq540,
      TRUE ~ 3),
    fastfood_usednutrit_fct = factor(fastfood_usednutrit,
                                 levels = c(1, 2, 3),
                                 labels = c("Yes", "No", "Missing")),
    fastfood_woulduse = case_when(
      cbq545 > 0 & cbq545 < 5 ~ cbq545,
      TRUE ~ 5),
    fastfood_woulduse_fct = factor(fastfood_woulduse,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Often", "Sometimes", "Rarely", "Never", "Missing")),
    restaur_eat = case_when(
      cbq550 > 0 & cbq550 < 3 ~ cbq550,
      TRUE ~ 3),
    restaur_eat_fct = factor(restaur_eat,
                         levels = c(1, 2, 3),
                         labels = c("Yes", "No", "Missing")),
    restaur_usednutrit = case_when(
      cbq585 > 0 & cbq585 < 3 ~ cbq585,
      TRUE ~ 3),
    restaur_usednutrit_fct = factor(restaur_usednutrit,
                                levels = c(1, 2, 3),
                                labels = c("Yes", "No", "Missing")),
    restaur_woulduse = case_when(
      cbq590 > 0 & cbq590 < 5 ~ cbq590,
      TRUE ~ 5),
    restaur_woulduse_fct = factor(restaur_woulduse,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Often", "Sometimes", "Rarely", "Never", "Missing")))

dietbehav <- dietbehav_stag %>% select(c("seqn", "diethealthy", "fastfood_eat", "fastfood_usednutrit", 
                                         "fastfood_woulduse", "restaur_eat", "restaur_usednutrit", "restaur_woulduse"))
dietbehav_fct <- dietbehav_stag %>% select(c("seqn", "diethealthy_fct", "fastfood_eat_fct", "fastfood_usednutrit_fct", 
                                             "fastfood_woulduse_fct", "restaur_eat_fct", "restaur_usednutrit_fct", 
                                             "restaur_woulduse_fct"))

# Dietary intake data, Day 1

names(dietintake_raw) <- str_to_lower(names(dietintake_raw))

dietintake_stag <- dietintake_raw %>% 
  select(seqn, dr1tkcal, dr1_300, dr1_320z) %>% 
  rename(dailykcal = dr1tkcal,
         dailywater = dr1_320z) %>% 
  mutate(
    dailykcal_typical = case_when(
      dr1_300 > 0 & dr1_300 < 4 ~ dr1_300,
      TRUE ~ 4),
    dailykcal_typical_fct = factor(dailykcal_typical,
                               levels = c(1, 2, 3, 4),
                               labels = c("Much more than usual",
                                          "Usual",
                                          "Much less than usual",
                                          "Missing")))

dietintake <- dietintake_stag %>% select(seqn, dailykcal, dailykcal_typical, dailywater)
dietintake_fct <- dietintake_stag %>% select(seqn, dailykcal, dailykcal_typical_fct, dailywater)

# Questionnaire data - PHQ-9

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
  mutate(PHQ9_score = rowSums(.[11:19], na.rm = FALSE),   # Ensuring only complete batteries are scored
         PHQ9_cat = case_when(
           PHQ9_score >=  0 & PHQ9_score <=  4 ~ 0,       # None to minimal
           PHQ9_score >=  5 & PHQ9_score <=  9 ~ 1,       # Mild
           PHQ9_score >= 10 & PHQ9_score <= 14 ~ 2,       # Moderate
           PHQ9_score >= 15 & PHQ9_score <= 19 ~ 3,       # Moderately severe
           PHQ9_score >= 20 & PHQ9_score <= 27 ~ 4,       # Severe
           TRUE ~ 5),
         PHQ9_cat_fct = factor(PHQ9_cat,
                           levels = c(0, 1, 2, 3, 4, 5),
                           labels = c("None to minimal",
                                      "Mild",
                                      "Moderate",
                                      "Moderately severe",
                                      "Severe",
                                      "Missing")))

PHQ9 <- PHQ9_stag %>% select(seqn, PHQ9_score, PHQ9_cat)
PHQ9_fct <- PHQ9_stag %>% select(seqn, PHQ9_score, PHQ9_cat_fct)

# Examination data - blood pressure

names(bloodpress_raw) <- str_to_lower(names(bloodpress_raw))

bloodpress_stag <- bloodpress_raw %>% 
  select(seqn, bpxsy2, bpxdi2) %>% 
  rename(systolic = bpxsy2,
         diastolic = bpxdi2) %>% 
  mutate(HTN_cat = case_when(
    systolic < 90 | diastolic < 60 ~ 0,                                               # Hypotension
    systolic >= 90 & systolic < 120 & diastolic >= 60 & diastolic < 80 ~ 1,           # Normal BP
    systolic >= 120 & diastolic < 80 ~ 2,                                             # Elevated BP
    (systolic >= 130 & systolic <= 139) | (diastolic >= 80 & diastolic <=  89) ~ 3,   # Stage 1 HTN
    (systolic >= 140 & systolic <= 180) | (diastolic >= 90 & diastolic <= 120) ~ 4,   # Stage 2 HTN
    systolic > 180 | diastolic > 120 ~ 5,                                             # Hypertensive crisis
    is.na(systolic) == TRUE | is.na(diastolic) == TRUE ~ 6),
  HTN_cat_fct = factor(HTN_cat,
                   levels = c(0, 1, 2, 3, 4, 5, 6),
                   labels = c("Hypotensive", "Normal", "Elevated", "Stage 1 HTN", "Stage 2 HTN", "Hypertensive crisis", "Missing")))

bloodpress <- bloodpress_stag %>% select(seqn, systolic, diastolic, HTN_cat)
bloodpress_fct <- bloodpress_stag %>% select(seqn, systolic, diastolic, HTN_cat_fct)

# Examination data - body measures

names(bodymeas_raw) <- str_to_lower(names(bodymeas_raw))

bodymeas_stag <- bodymeas_raw %>% 
  select(seqn, bmxwt, bmxht, bmxbmi) %>% 
  rename(weight_kg = bmxwt,
         height_cm = bmxht,
         BMI = bmxbmi) %>% 
  mutate(
    BMI_cat = case_when(
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 25.0 ~ 2,
      BMI >= 25.0 & BMI < 30.0 ~ 3,
      BMI >= 30.0 ~ 4,
      is.na(BMI) == TRUE ~ 5),
    BMI_cat_fct = factor(BMI_cat,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Underweight", "Normal weight", "Overweight", "Obese", "Missing")))
  
bodymeas <- bodymeas_stag %>% select(seqn, BMI, BMI_cat)
bodymeas_fct <- bodymeas_stag %>% select(seqn, BMI, BMI_cat_fct)

# Demographics data

names(demog_raw) <- str_to_lower(names(demog_raw))

demog_stag <- demog_raw %>% 
  select(seqn, riagendr, ridageyr, ridreth3, dmdeduc2, dmdmartl, indfmin2, indfmpir) %>% 
  rename(gender = riagendr,
         age = ridageyr,
         race = ridreth3,
         famincome_povratio = indfmpir) %>% 
  mutate(gender_fct = factor(gender,
                         levels = c(1, 2),
                         labels = c("Male", "Female")),
         race = case_when(
           race == 1 | race == 2 ~ 1,
           TRUE ~ race),
         race_fct = factor(race,
                       levels = c(1, 3, 4, 6, 7),
                       labels = c("Hispanic",
                                  "Non-Hispanic white",
                                  "Non-Hispanic black",
                                  "Non-Hispanic Asian",
                                  "Other or mixed race")),
         educ = case_when(
           dmdeduc2 > 0 & dmdeduc2 < 6 ~ dmdeduc2,
           TRUE ~ 6),
         educ_fct = factor(educ,
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
         marital_fct = factor(marital,
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
         famincome_cat_fct = factor(famincome_cat,
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
                                           "Missing")))          # 16 (comprises 12, 13, 77, 99, and missing)

demog <- demog_stag %>% select(c("seqn", "age", "famincome_povratio", "gender", "race", "educ", 
                                 "marital", "famincome_cat"))
demog_fct <- demog_stag %>% select(c("seqn", "age", "famincome_povratio", "gender_fct", "race_fct", 
                                     "educ_fct", "marital_fct", "famincome_cat_fct"))

# Questionnaire data - diabetes items

names(diabetes_raw) <- str_to_lower(names(diabetes_raw))

diabetes_stag <- diabetes_raw %>% 
  select(seqn, diq010, diq280) %>% 
  mutate(
    diabet_hx = case_when(
      diq010 > 0 & diq010 < 4 ~ diq010,
      TRUE ~ 4),
    diabet_hx_fct = factor(diabet_hx,
                       levels = c(1, 2, 3, 4),
                       labels = c("Yes", "No", "Borderline", "Missing")),
    hba1c = case_when(
      diq280 > 18.5 ~ as.numeric(NA),
      TRUE ~ diq280))

diabetes <- diabetes_stag %>% select(seqn, diabet_hx, hba1c)
diabetes_fct <- diabetes_stag %>% select(seqn, diabet_hx_fct, hba1c)

# Questionnaire data - medical items

names(medical_raw) <- str_to_lower(names(medical_raw))

medical_stag <- medical_raw %>% 
  select(seqn, mcq160c, mcq160e, mcq160m, mcq365a, mcq365b) %>% 
  mutate(
    CAD_hx = case_when(
      mcq160c > 0 & mcq160c < 3 ~ mcq160c,
      TRUE ~ 3),
    CAD_hx_fct = factor(CAD_hx,
                    levels = c(1, 2, 3),
                    labels = c("Yes", "No", "Missing")),
    MI_hx = case_when(
      mcq160e > 0 & mcq160e < 3 ~ mcq160e,
      TRUE ~ 3),
    MI_hx_fct = factor(MI_hx,
                   levels = c(1, 2, 3),
                   labels = c("Yes", "No", "Missing")),
    thy_hx = case_when(
      mcq160m > 0 & mcq160m < 3 ~ mcq160m,
      TRUE ~ 3),
    thy_hx_fct = factor(thy_hx,
                    levels = c(1, 2, 3),
                    labels = c("Yes", "No", "Missing")),
    doc_losewt = case_when(
      mcq365a > 0 & mcq365a < 3 ~ mcq365a,
      TRUE ~ 3),
    doc_losewt_fct = factor(doc_losewt,
                        levels = c(1, 2, 3),
                        labels = c("Yes", "No", "Missing")),
    doc_exer = case_when(
      mcq365b > 0 & mcq365b < 3 ~ mcq365b,
      TRUE ~ 3),
    doc_exer_fct = factor(doc_exer,
                      levels = c(1, 2, 3),
                      labels = c("Yes", "No", "Missing")))

medical <- medical_stag %>% select(c("seqn", "CAD_hx", "MI_hx", "thy_hx", "doc_losewt", "doc_exer"))
medical_fct <- medical_stag %>% select(c("seqn", "CAD_hx_fct", "MI_hx_fct", "thy_hx_fct", "doc_losewt_fct", "doc_exer_fct"))

# Questionnaire data - physical activity

names(physactiv_raw) <- str_to_lower(names(physactiv_raw))

physactiv_stag <- physactiv_raw %>% 
  select(seqn, pad615, pad630, pad660, pad675, pad680) %>% 
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
  mutate(mins_activ = sum(mins_vigwork, mins_modwork, mins_vigrec, mins_modrec, na.rm = TRUE))

physactiv <- physactiv_stag %>% select(seqn, mins_activ, mins_seden)

# Questionnaire data - physical functioning

names(physfxn_raw) <- str_to_lower(names(physfxn_raw))

physfxn_stag <- physfxn_raw %>% 
  select(seqn, pfq049, pfq061b) %>% 
  mutate(
    worklim = case_when(
      pfq049 > 0 & pfq049 < 3 ~ pfq049,
      TRUE ~ 3),
    worklim_fct = factor(worklim,
                     levels = c(1, 2, 3),
                     labels = c("Yes", "No", "Missing")),
    walklim = case_when(
      pfq061b > 0 & pfq061b < 6 ~ pfq061b,
      TRUE ~ 6),
    walklim_fct = factor(walklim,
                     levels = c(1, 2, 3, 4, 5, 6),
                     labels = c("No difficulty", "Some difficulty", "Much difficulty", "Unable to do", 
                                "Do not do this activity", "Missing")))

physfxn <- physfxn_stag %>% select(seqn, worklim, walklim)
physfxn_fct <- physfxn_stag %>% select(seqn, worklim_fct, walklim_fct)

# Removing raw dataframes

rm(list = c("dietbehav_raw", "dietintake_raw", "PHQ9_raw", "bloodpress_raw", "bodymeas_raw", "demog_raw",
          "diabetes_raw", "medical_raw", "physactiv_raw", "physfxn_raw"))


# Joining datasets

a <- bloodpress$seqn %>% as_tibble()
b <- bodymeas$seqn %>% as_tibble()
c <- demog$seqn %>% as_tibble()
d <- diabetes$seqn %>% as_tibble()
e <- dietbehav$seqn %>% as_tibble()
f <- dietintake$seqn %>% as_tibble()
g <- medical$seqn %>% as_tibble()
h <- PHQ9$seqn %>% as_tibble()
i <- physactiv$seqn %>% as_tibble()
j <- physfxn$seqn %>% as_tibble()

allseqn <- rbind(a, b, c, d, e, f, g, h, i, j) %>% 
  unique() %>% 
  arrange() %>% 
  rename(seqn = value)

rm(list = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))

nhanes <- allseqn %>%   # These joins will throw a warning; ignore it - this is a known dplyr issue
  left_join(demog, by = c("seqn" = "seqn")) %>% 
  left_join(diabetes, by = c("seqn" = "seqn")) %>% 
  left_join(medical, by = c("seqn" = "seqn")) %>% 
  left_join(bloodpress, by = c("seqn" = "seqn")) %>% 
  left_join(physactiv, by = c("seqn" = "seqn")) %>% 
  left_join(physfxn, by = c("seqn" = "seqn")) %>% 
  left_join(PHQ9, by = c("seqn" = "seqn")) %>% 
  left_join(dietbehav, by = c("seqn" = "seqn")) %>% 
  left_join(dietintake, by = c("seqn" = "seqn")) %>% 
  left_join(bodymeas, by = c("seqn" = "seqn")) %>% 
  select(seqn, age, gender, race, educ, marital, famincome_cat, famincome_povratio,
         diabet_hx, hba1c, CAD_hx, MI_hx, thy_hx, doc_losewt, doc_exer,
         systolic, diastolic, HTN_cat,
         mins_activ, mins_seden, worklim, walklim,
         diethealthy, fastfood_eat, fastfood_usednutrit, fastfood_woulduse, restaur_eat, restaur_usednutrit, restaur_woulduse,
         dailykcal, dailykcal_typical, dailywater,
         PHQ9_score, PHQ9_cat,
         BMI, BMI_cat) %>% 
  remove_all_labels() %>%   # Removing annoying column labels
  filter(age >= 20)

# Generating nhanes data with factors

nhanes_fct <- allseqn %>%   # These joins will throw a warning; ignore it - this is a known dplyr issue
  left_join(demog_fct, by = c("seqn" = "seqn")) %>% 
  left_join(diabetes_fct, by = c("seqn" = "seqn")) %>% 
  left_join(medical_fct, by = c("seqn" = "seqn")) %>% 
  left_join(bloodpress_fct, by = c("seqn" = "seqn")) %>% 
  left_join(physactiv, by = c("seqn" = "seqn")) %>% 
  left_join(physfxn_fct, by = c("seqn" = "seqn")) %>% 
  left_join(PHQ9_fct, by = c("seqn" = "seqn")) %>% 
  left_join(dietbehav_fct, by = c("seqn" = "seqn")) %>% 
  left_join(dietintake_fct, by = c("seqn" = "seqn")) %>% 
  left_join(bodymeas_fct, by = c("seqn" = "seqn")) %>% 
  select(seqn, age, gender_fct, race_fct, educ_fct, marital_fct, famincome_cat_fct, famincome_povratio,
         diabet_hx_fct, hba1c, CAD_hx_fct, MI_hx_fct, thy_hx_fct, doc_losewt_fct, doc_exer_fct,
         systolic, diastolic, HTN_cat_fct,
         mins_activ, mins_seden, worklim_fct, walklim_fct,
         diethealthy_fct, fastfood_eat_fct, fastfood_usednutrit_fct, fastfood_woulduse_fct, restaur_eat_fct, 
         restaur_usednutrit_fct, restaur_woulduse_fct,
         dailykcal, dailykcal_typical_fct, dailywater,
         PHQ9_score, PHQ9_cat_fct,
         BMI, BMI_cat_fct) %>% 
  remove_all_labels() %>%   # Removing annoying column labels
  filter(age >= 20)


#### Exploratory analysis ----

# Continuous variables

nhanes_contin <- nhanes_fct %>%
  select(age, famincome_povratio, hba1c, systolic, diastolic, mins_activ, mins_seden, dailykcal, dailywater, PHQ9_score, BMI) %>% 
  describe()

contins <- rownames(nhanes_contin)

nhanes_contin_kable <- nhanes_contin %>% 
  mutate(vars = contins) %>% 
  select(vars, n, mean, sd, min, max, median) %>% 
  mutate(mean = round(mean, digits = 2),
         sd = round(sd, digits = 3)) %>% 
  kable(format = "markdown")

# Categorical variables

nhanes_categvars <- nhanes_fct %>%
  select(gender_fct, race_fct, educ_fct, marital_fct, famincome_cat_fct, diabet_hx_fct, CAD_hx_fct, 
         MI_hx_fct, thy_hx_fct, doc_losewt_fct, doc_exer_fct, HTN_cat_fct, worklim_fct, walklim_fct, 
         diethealthy_fct, fastfood_eat_fct, fastfood_usednutrit_fct, fastfood_woulduse_fct, restaur_eat_fct, 
         restaur_usednutrit_fct, restaur_woulduse_fct, dailykcal_typical_fct, PHQ9_cat_fct, BMI_cat_fct) %>% 
  names()

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
HTN_cat_summ <- categsumm(nhanes_fct, quo(HTN_cat_fct))
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


#### Handling all missing values

# nhanes <- nhanes %>% 
#   select(-hba1c) %>% 
#   filter(!is.na(BMI) == TRUE) 
  


#### Rendering .Rmd ----
# render("checkpoints/Prioli_checkpoint1.Rmd")
# render("checkpoints/Prioli_checkpoint2.Rmd")
# render("checkpoints/Prioli_checkpoint3.Rmd")
# render("Prioli_final_report.Rmd")