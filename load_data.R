# Katherine M. Prioli
# CSC 8515 Final Project - load analytic datasets from .RData
# Sat Nov 30 19:57:35 2019 ------------------------------


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
library(GGally)         # For ggpairs()
library(ggthemr)        # For prettifying output plots
library(cowplot)        # For ggdraw() and draw_plot()
library(magick)         # For image_read_svg()
library(tidyselect)     # For selecting by string
library(tidyverse)      # For data import and wrangling

ggthemr("flat")
set.seed(20191205)      # Setting seed to ensure stable results


#### Loading staging files (avoids rerunning computationally intense code)

load("data/staging/analytic_datasets.RData")
load("data/staging/random_forest_init_scores.RData")   # Created 11.10.2019 w/seed 20191110; see git commit a9a7880 to generate corresponding dataset
load("data/staging/random_forest_final_scores.RData")
load("data/staging/clustering_stats.RData")


#### Rendering final report .Rmd ----

# render("report/Prioli_final_report.Rmd")   # Run this after running all chunks in clustering_analysis.Rmd