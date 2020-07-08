rm(list=ls())

##### 1.0 Global settings ------------------------------------------------------------------------------------------------------------
### 1.1 Options
options(scipen=999)

### 1.2 Set seed
set.seed(160494)

### 1.3 Encoding
Sys.setlocale(category = "LC_ALL", locale = "Danish")

##### 2.0 Dependencies ---------------------------------------------------------------------------------------------------------------
### 2.1 Get pacman
library(pacman)

### 1.2 Get other dependencies
pacman::p_load(dplyr, tidyverse, ggplot2, haven, stargazer, xlsx, psych, sjstats, 
               lubridate, foreign, broom, stringr, purrr, jsonlite, data.table, beepr,
               rlang, scales, magrittr, survey, corrplot, openxlsx, cregg, forcats, 
               cjoint, grid, ggpubr, estimatr, emmeans, gridExtra, knitr, readr)


##### 3.0 Functions ------------------------------------------------------------------------------------------------------------------
### 3.1 Colors
source("00. Globale indstillinger/epinion_color.R")

# 3.2 Theme
source("00. Globale indstillinger/speciale_style.R")
