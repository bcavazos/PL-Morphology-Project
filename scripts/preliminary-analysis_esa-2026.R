## -------------------------------------------------------- ##
# Preliminary Analysis - ESA 2026
## -------------------------------------------------------- ##
# Purpose:
## Preliminary Analysis of PL Data for ESA Abstract
## Three morphological traits are "leaf_area_cm2", "infl_length_cm", and "int_length_cm"
## We want to know how these change in native vs invasive ranges (range column)

# Hypothesis:
## We hypothesized leaf area, inflorescence length and internode length all 
### might increase in invasive ranges due to release from competition, 
### but some groups found interesting patterns suggesting a tradeoff
### (e.g., longer internode length but smaller leaf area)
## Eventually we want to see if latitude is an important factor as well, (and year of record)

# Load libraries
## install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Create necessary folders
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------ ##
# Initial Wrangling ----
## ------------------------------------------ ##

# Read in data
pl_v01 <- read.csv(file.path("data", "PLdata_full.02.17.26.csv"))

# Check structure
dplyr::glimpse(pl_v01)

# Do some quality control (qc) checks
## Any typos in range?
sort(unique(pl_v01$range))
## Value range for response vars all seem reasonable?
hist(x = pl_v01$leaf_area_cm2)
hist(x = pl_v01$infl_length_cm)
hist(x = pl_v01$int_length_cm)

# Do any needed repairs
pl_v02 <- pl_v01 %>%
  dplyr::mutate(range = dplyr::case_when(
    range %in% c("invasive",  "invasive ",  "invasive ", "onvasive ") ~ "invasive",
    range %in% c( "native", "native ") ~ "native",
    T ~ range)) %>% 
  # Keep only known native/invasive range
  dplyr::filter(range %in% c("native", "invasive"))

# Re-check QC efforts
sort(unique(pl_v02$range))

# General structure check
dplyr::glimpse(pl_v02)

## ------------------------------------------ ##
# Test Core Hypotheses ----
## ------------------------------------------ ##

# Leaf area test
lfar_mod <- lm(leaf_area_cm2 ~ range, data = pl_v02)
stats::anova(lfar_mod)

# Inflorescence length test
infl_mod <- lm(infl_length_cm ~ range, data = pl_v02)
stats::anova(infl_mod)

# Internode length test
node_mod <- lm(int_length_cm ~ range, data = pl_v02)
stats::anova(node_mod)

## ------------------------------------------ ##
# Summarize & Graph Results ----
## ------------------------------------------ ##

# Get summarized versions of the data for each response variable
(lfar_mean <- supportR::summary_table(data = pl_v02, groups = "range",
  response = "leaf_area_cm2", drop_na = T))
(infl_mean <- supportR::summary_table(data = pl_v02, groups = "range",
  response = "infl_length_cm", drop_na = T))
(node_mean <- supportR::summary_table(data = pl_v02, groups = "range",
  response = "int_length_cm", drop_na = T))

# Make some graphs!



# End ----
