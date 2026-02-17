###################################
# Brittany Cavazos
# Feb 11 2026
# Merge all 4 groups' data into one data set so Jules can fill in missing pieces
###################################

# set wd
setwd("~/OneDrive - Stonehill College/UndergradResearch/Jules-MorphologyProject")
  # different on your computer

# load libraries
library(tidyverse)

# read in data
apapane <- read.csv("data/raw data/apapane_data.csv")
iiwi <- read.csv("data/raw data/iiwi_data.csv")
nene <- read.csv("data/raw data/nene_data.csv")
pueo <- read.csv("data/raw data/pueo_data.csv")

# check that column names are the same

# datasets with infl length
setdiff(names(apapane), names(pueo))
setdiff(names(pueo), names(apapane))

# datasets w/ internode
setdiff(names(iiwi), names(nene))
setdiff(names(nene), names(iiwi))

# make object that lists all the new_name = oldname
names <- c(range = "Range", 
      leaf_area_cm2 = "Leaf_Area_cm2", 
      leaf_area_cm2 = "leaf_area_cm.2", 
      leaf_area_cm2 = "leaf_area", 
      infl_length_cm = "infl_length",
      date_collected = "date_processed",
      pixelpercm = "pixel.cm")

# standardize column names across four groups
pueo <- pueo %>%
  dplyr::rename(any_of(names)) %>%
  select(-Processed.) # get rid of processed column

apapane <- apapane %>%
  dplyr::rename(any_of(names))

iiwi <- iiwi %>%
  dplyr::rename(any_of(names))

nene <- nene %>%
  dplyr::rename(any_of(names))

# combine all four data sets 
# should end with a data set that is 19 variables long and 947 rows long
join1 <- full_join(apapane, pueo)
join2 <- full_join(join1, nene)
finaljoin <- full_join(join2, iiwi)

# fix ordering of columns
finaljoin <- finaljoin %>%
  relocate(int_length_cm, .before = initials)

# write csv
write.csv(finaljoin, "data/raw data/PLdata_full.02.17.26.csv")
 
