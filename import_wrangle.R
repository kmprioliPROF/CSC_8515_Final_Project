# Katherine M. Prioli
# CSC 8515 Final Project
# Sat Nov 02 17:04:28 2019 ------------------------------


#### Loading libraries ----

library(tidyverse)    # For data import and wrangling
library(haven)        # For loading SAS .xpt files
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

# Questionnaire data - dietary behavior

names(dietbehav_raw) <- str_to_lower(names(dietbehav_raw))

dietbehav_stag <- dietbehav_raw %>%
  select(seqn, dbq700, cbq505, cbq540, cbq545, cbq550, cbq585, cbq590) %>%
  mutate(
    diethealthy = case_when(
      dbq700 > 5 ~ as.numeric(NA),
      TRUE ~ dbq700),
    diethealthy = factor(diethealthy,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    fastfood_eat = case_when(
      cbq505 > 3 ~ as.numeric(NA),
      TRUE ~ cbq505),
    fastfood_eat = factor(fastfood_eat,
                          levels = c(1, 2),
                          labels = c("Yes", "No")),
    fastfood_usednutrit = case_when(
      cbq540 > 3 ~ as.numeric(NA),
      TRUE ~ cbq540),
    fastfood_usednutrit = factor(fastfood_usednutrit,
                                 levels = c(1, 2),
                                 labels = c("Yes", "No")),
    fastfood_woulduse = case_when(
      cbq545 > 4 ~ as.numeric(NA),
      TRUE ~ cbq545),
    fastfood_woulduse = factor(fastfood_woulduse,
                               levels = c(1, 2, 3, 4),
                               labels = c("Often", "Sometimes", "Rarely", "Never")),
    restaur_eat = case_when(
      cbq550 > 3 ~ as.numeric(NA),
      TRUE ~ cbq550),
    restaur_eat = factor(restaur_eat,
                         levels = c(1, 2),
                         labels = c("Yes", "No")),
    restaur_usednutrit = case_when(
      cbq585 > 3 ~ as.numeric(NA),
      TRUE ~ cbq585),
    restaur_usednutrit = factor(restaur_usednutrit,
                                levels = c(1, 2),
                                labels = c("Yes", "No")),
    restaur_woulduse = case_when(
      cbq590 > 4 ~ as.numeric(NA),
      TRUE ~ cbq590),
    restaur_woulduse = factor(restaur_woulduse,
                              levels = c(1, 2, 3, 4),
                              labels = c("Often", "Sometimes", "Rarely", "Never")))

dietbehav <- dietbehav_stag %>% select(-dbq700, -cbq505, -cbq540, -cbq545, -cbq550, -cbq585, -cbq590)

# Dietary intake data, Day 1

names(dietintake_raw) <- str_to_lower(names(dietintake_raw))

dietintake_stag <- dietintake_raw %>% 
  select(seqn, dr1tkcal, dr1_300, dr1_320z) %>% 
  rename(dailykcal = dr1tkcal,
         dailywater = dr1_320z) %>% 
  mutate(
    dailykcal_typical = case_when(
      dr1_300 > 3 ~ as.numeric(NA),
      TRUE ~ dr1_300),
    dailykcal_typical = factor(dailykcal_typical,
                               levels = c(1, 2, 3),
                               labels = c("Much more than usual",
                                          "Usual",
                                          "Much less than usual")))

dietintake <- dietintake_stag %>% select(-dr1_300)

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
           TRUE ~ as.numeric(NA)),
         PHQ9_cat = factor(PHQ9_cat,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("None to minimal",
                                      "Mild",
                                      "Moderate",
                                      "Moderately severe",
                                      "Severe")))

PHQ9 <- PHQ9_stag %>% select(seqn, PHQ9_score, PHQ9_cat)

# Examination data - blood pressure

names(bloodpress_raw) <- str_to_lower(names(bloodpress_raw))

bloodpress <- bloodpress_raw %>% 
  select(seqn, bpxsy2, bpxdi2) %>% 
  rename(systolic = bpxsy2,
         diastolic = bpxdi2) %>% 
  mutate(HTN_cat = case_when(
    is.na(systolic) == TRUE | is.na(diastolic) == TRUE ~ as.numeric(NA),
    systolic <  120 & diastolic < 80 ~ 0,   # Normal BP
    systolic >= 120 & diastolic < 80 ~ 1,   # Elevated BP
    (systolic >= 130 & systolic <= 139) | (diastolic >= 80 & diastolic <=  89) ~ 2,   # Stage 1 HTN
    (systolic >= 140 & systolic <= 180) | (diastolic >= 90 & diastolic <= 120) ~ 3,   # Stage 2 HTN
    systolic > 180 | diastolic > 120 ~ 4   # Hypertensive crisis
  ),
  HTN_cat = factor(HTN_cat,
                   levels = c(0, 1, 2, 3, 4),
                   labels = c("Normal", "Elevated", "Stage 1 HTN", 
                              "Stage 2 HTN", "Hypertensive crisis")))

# Examination data - body measures

names(bodymeas_raw) <- str_to_lower(names(bodymeas_raw))

bodymeas_stag <- bodymeas_raw %>% 
  select(seqn, bmxwt, bmxht, bmxbmi) %>% 
  rename(weight_kg = bmxwt,
         height_cm = bmxht,
         BMI = bmxbmi) %>% 
  mutate(BMI_cat = case_when(
    is.na(BMI) == TRUE ~ as.numeric(NA),
    BMI < 18.5 ~ 1,                 # Underweight
    BMI >= 18.5 & BMI < 25.0 ~ 2,   # Normal weight
    BMI >= 25.0 & BMI < 30.0 ~ 3,   # Overweight
    BMI >= 30.0 ~ 4                 # Obese
  ))
  
bodymeas <- bodymeas_stag %>% select(-weight_kg, -height_cm)

# Demographics data

names(demog_raw) <- str_to_lower(names(demog_raw))

demog_stag <- demog_raw %>% 
  select(seqn, riagendr, ridageyr, ridreth3, dmdeduc2, dmdmartl, indfmin2, indfmpir) %>% 
  rename(age = ridageyr,
         famincome_povratio = indfmpir) %>% 
  mutate(gender = factor(riagendr,
                         levels = c(1, 2),
                         labels = c("Male", "Female")),
         race = factor(ridreth3,
                       levels = c(1, 2, 3, 4, 6, 7),
                       labels = c("Mexican American",
                                  "Other Hispanic",
                                  "Non-Hispanic white",
                                  "Non-Hispanic black",
                                  "Non-Hispanic Asian",
                                  "Other or mixed race")),
         educ = case_when(
           dmdeduc2 > 5 ~ as.numeric(NA),
           TRUE ~ dmdeduc2),
         educ = factor(educ,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("Less than 9th grade",
                                  "9th-12th grade, no HS diploma",
                                  "High school / GED",
                                  "Some college / AA degree",
                                  "College graduate or above")),
         marital = case_when(
           dmdmartl > 6 ~ as.numeric(NA),
           TRUE ~ dmdmartl),
         marital = factor(marital,
                          levels = c(1, 2, 3, 4, 5, 6),
                          labels = c("Married",
                                     "Widowed",
                                     "Divorced",
                                     "Separated",
                                     "Never married",
                                     "Living with partner")),
         famincome_cat = case_when(
           (indfmin2 >= 11 & indfmin2 <= 13) | indfmin2 > 15 ~ as.numeric(NA),
           TRUE ~ indfmin2),
         famincome_cat = factor(famincome_cat,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15),
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
                                           "$100,000 and over")))  # 15

demog <- demog_stag %>% select(-riagendr, -ridreth3, -dmdeduc2, -dmdmartl, -indfmin2)

# Questionnaire data - diabetes items

names(diabetes_raw) <- str_to_lower(names(diabetes_raw))

diabetes_stag <- diabetes_raw %>% 
  select(seqn, diq010, diq280) %>% 
  mutate(
    diabet_hx = case_when(
      diq010 > 3 ~ as.numeric(NA),
      TRUE ~ diq010),
    diabet_hx = factor(diabet_hx,
                       levels = c(1, 2, 3),
                       labels = c("Yes", "No", "Borderline")),
    hba1c = case_when(
      diq280 > 18.5 ~ as.numeric(NA),
      TRUE ~ diq280))

diabetes <- diabetes_stag %>% select(-diq010, -diq280)

# Questionnaire data - medical items

names(medical_raw) <- str_to_lower(names(medical_raw))

medical_stag <- medical_raw %>% 
  select(seqn, mcq160c, mcq160e, mcq160m, mcq365a, mcq365b) %>% 
  mutate(
    CAD_hx = case_when(
      mcq160c > 2 ~ as.numeric(NA),
      TRUE ~ mcq160c),
    CAD_hx = factor(CAD_hx,
                    levels = c(1, 2),
                    labels = c("Yes", "No")),
    MI_hx = case_when(
      mcq160e > 2 ~ as.numeric(NA),
      TRUE ~ mcq160e),
    MI_hx = factor(MI_hx,
                   levels = c(1, 2),
                   labels = c("Yes", "No")),
    thy_hx = case_when(
      mcq160m > 2 ~ as.numeric(NA),
      TRUE ~ mcq160m),
    thy_hx = factor(thy_hx,
                    levels = c(1, 2),
                    labels = c("Yes", "No")),
    doc_losewt = case_when(
      mcq365a > 2 ~ as.numeric(NA),
      TRUE ~ mcq365a),
    doc_losewt = factor(doc_losewt,
                        levels = c(1, 2),
                        labels = c("Yes", "No")),
    doc_exer = case_when(
      mcq365b > 2 ~ as.numeric(NA),
      TRUE ~ mcq365b),
    doc_exer = factor(doc_exer,
                      levels = c(1, 2),
                      labels = c("Yes", "No")))

medical <- medical_stag %>% select(-mcq160c, -mcq160e, -mcq160m, -mcq365a, -mcq365b)

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
      pad675 > 660 ~ as.numeric(NA),
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
      pfq049 > 2 ~ as.numeric(NA),
      TRUE ~ pfq049),
    worklim = factor(worklim,
                     levels = c(1, 2),
                     labels = c("Yes", "No")),
    walklim = case_when(
      pfq061b > 5 ~ as.numeric(NA),
      TRUE ~ pfq061b),
    walklim = factor(walklim,
                     levels = c(1, 2, 3, 4),
                     labels = c("No difficulty", "Some difficulty", "Much difficulty", "Unable to do")))

physfxn <- physfxn_stag %>% select(-pfq049, -pfq061b)

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
         BMI, BMI_cat)


#### Exploratory analysis ----

# Continuous variables

nhanes_contin <- nhanes %>%
  select(age, famincome_povratio, hba1c, systolic, diastolic, mins_activ, mins_seden, dailykcal, dailywater, PHQ9_score, BMI)

nhanes_contin_kable <- nhanes_contin %>% 
  describe() %>% 
  select(n, mean, sd, min, max, median) %>% 
  mutate(mean = round(mean, digits = 2),
         sd = round(sd, digits = 3)) %>% 
  kable(format = "markdown")

nhanes_categvars <- nhanes %>%
  select(gender, race, educ, marital, famincome_cat, diabet_hx, CAD_hx, MI_hx, thy_hx, doc_losewt, doc_exer,
         HTN_cat, worklim, walklim, diethealthy, fastfood_eat, fastfood_usednutrit, fastfood_woulduse,
         restaur_eat, restaur_usednutrit, restaur_woulduse, PHQ9_cat, BMI_cat) %>%
  names()

categsumm <- function(df, x){
  summdf <- df %>%
    group_by(!! x) %>% 
    select(!! x) %>% 
    summarize(n = n(),
              pct = round((n / dim(df)[1]) * 100, digits = 2)) %>% 
    arrange(desc(n))
  dfout <- return(summdf)
}

for(i in 1:length(nhanes_categvars)){
  summ <- categsumm(nhanes, rlang::quo_get_expr(quo(nhanes_categvars[[1]])))
  summout <- return(summ)
}




#### CONTINUE HERE ----
# Need to get the for-loop working because there's no way I'm running this fxn for each categ var!




gender_summ <- categsumm(nhanes, quo(gender))
race_summ <- categsumm(nhanes, quo(race))
educ_summ <- categsumm(nhanes, quo(educ))
famincome_cat_summ <- categsumm(nhanes, quo(famincome_cat))
diabet_hx_summ <- categsumm(nhanes, quo(diabet_hx))
CAD_hx_summ <- categsumm(nhanes, quo(CAD_hx))
MI_hx_summ <- categsumm(nhanes, quo(MI_hx))
thy_hx_summ <- categsumm(nhanes, quo(thy_hx))
doc_losewt_summ <- categsumm(nhanes, quo(MI_hx))




#### Sending data to .Rmd ----
# render("Prioli_final_report.Rmd")