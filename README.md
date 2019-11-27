---
title: "README"
author: "Katherine M. Prioli"
date: "December 05, 2019"
output: html_document
---

  
## **Overview**
  
This repo contains the data, full source code, final report, and slides for my CSC 8515 final project, which used supervised and unsupervised machine learning approaches to explore obesity and its correlates.

## **Background**

Overweight and obesity are growing public health concerns, with 71.6% of American adults are overweight and 39.8% obese.  Obesity strongly correlates with increased morbidity and mortality and the economic burden of obesity is high, both for direct costs (e.g., healthcare utilization) and indirect costs (e.g., work productivity loss).  The National Health and Nutrition Examination Survey (NHANES) is a biennial health surveillance study performed by the Centers for Disease Control and Prevention (CDC) that seeks to understand population-level health by gathering demographic, medical, mental health, and behavioral data through self-report and physical examination.  NHANES has uncovered some demographic correlates of obesity, but much remains unknown about the interactions of these correlates and how constellations thereof may be used to identify obesity at the population level.

Using a carefully selected a carefully selected set of variables from the 2015-2016 NHANES data which are known or suspected to be correlated with obesity, the objectives of this study were:

1.  Run a Random Forest using an $n \times k$-fold crossvalidation approach to classify cases into weight categories by Body Mass Index (BMI)
2.  Apply agglomerative and divisive hierarchical clustering methods (AGNES and DIANA, respectively) to unlabeled data and plot the corresponding dendrograms along with the BMI category labels to determine how well the clustering approach is able to identify similar cases


## **Repo Contents**

The repo is organized as follows:
  
  | **Content**                  | **Description**                                                    	                                      |
  |------------------------------|----------------------------------------------------------------------------------------------------------- |
  | checkpoints/                 | Contains intermediate "checkpoint" project status reports                                                  |
  | data/                        | Contains source datasets used in the analysis                                                              |
  | presentation/                | Contains final presentation slides and any supporting files                                                |
  | CSC_8515_final_project.Rproj | .Rproj project container                                                                                   |
  | clustering_analysis.Rmd      | RMarkdown notebook containing the hierarchical clustering analysis                                         |
  | clustering_stats.R           | R script used to generate clustering statistics for scree and silhouette plots                             |
  | import_wrangle.R             | R script used to import raw data and generate analytic datasets; **_must be run before all other files_**  |
  | Prioli_final_report.Rmd      | RMarkdown notebook containing final report                                                                 |
  | Prioli_final_report.pdf      | .pdf of final report                                                                                       |
  | random_forest.Rmd            | RMarkdown notebook containing the $n \times k$-fold crossvalidation Random Forest analysis                 |


## **Technologies and Dependencies**

This project is written largely in R and uses .R and .Rmd files; however, the Random Forest analysis is performed in Python 3.8.0 and leverages the libraries `math`, `numpy`, `pandas`, and `sklearn`.

The R components of this analysis rely on the following packages:

* `broom`
* `cluster`
* `dendextend`
* `forcats`
* `fpc`
* `ggdendro`
* `ggthemr`
* `grid`
* `gridExtra`
* `haven`
* `here`
* `kableExtra`
* `psych`
* `randomForest`
* `reticulate`
* `rmarkdown`
* `sjlabelled`
* `tidyselect`
* `tidyverse`
* `vegan`

If you need any of these, you can install them by modifying and running the code below as needed:

```
install.packages(c("broom",
                   "cluster",
                   "dendextend",
                   "forcats",
                   "fpc",
                   "ggdendro",
                   "ggthemr",
                   "grid",
                   "gridExtra",
                   "haven",
                   "here",
                   "kableExtra",
                   "psych",
                   "randomForest",
                   "reticulate",
                   "rmarkdown",
                   "sjlabelled",
                   "tidyselect",
                   "tidyverse",
                   "vegan"))
```


## **Usage**

The script `import_wrangle.R` must be run before any other file.  It will take about 5 minutes to run (there is a computationally intense imputation step).  From there, you can run either `random_forest.Rmd` for the supervised analysis, or `clustering_analysis.Rmd` for the unsupervised analysis.  The script `clustering_stats.R` is automatically called from within `clustering_analysis.Rmd` so no need to run it separately.


## **Data Source**

All data used in this analysis is from the 2015-2016 NHANES dataset, available from the National Center for Health Statistics:  https://www.cdc.gov/nchs/nhanes/index.htm.