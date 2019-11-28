# Katherine M. Prioli
# CSC 8515 Final Project - load analytic datasets from .RData
# Wed Nov 27 21:13:36 2019 ------------------------------


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
set.seed(20191205)      # Setting seed to ensure stable results

load("data/analytic_datasets.RData")