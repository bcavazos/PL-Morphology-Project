## -------------------------------------------------------- ##
# Preliminary Analysis - ESA 2026
## -------------------------------------------------------- ##
# Purpose:
## Preliminary Analysis of PL Data for ESA Abstract
## Three morphological traits are "leaf_area_cm2", "infl_length_cm", and "int_length"
## We want to know how these change in native vs invasive ranges (range column)

# Hypothesis:
## We hypothesized leaf area, inflorescence length and internode length all 
### might increase in invasive ranges due to release from competition, 
### but some groups found interesting patterns suggesting a tradeoff
### (e.g., longer internode length but smaller leaf area)
## Eventually we want to see if latitude is an important factor as well, (and year of record)

# Load libraries
## install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------ ##
# Initial Wrangling ----
## ------------------------------------------ ##

# Read in data
pl_v01 <- read.csv(file.path("data", "PLdata_full.02.17.26.csv"))

# Check structure
dplyr::glimpse(pl_v01)






# End ----
