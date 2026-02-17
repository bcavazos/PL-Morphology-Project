###################################
# Brittany Cavazos
# Feb 11 2026
# Merge all 4 groups' data into one data set so Jules can fill in missing pieces
###################################

# set wd
setwd("~/OneDrive - Stonehill College/UndergradResearch/Jules-MorphologyProject")
  # different on your computer

# load libraries
## install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Make needed folder(s)
dir.create(path = file.path("data", "raw data"), showWarnings = F, recursive = T)

# read in data
apapane <- read.csv(file.path("data", "raw data", "apapane_data.csv"))
iiwi <- read.csv(file.path("data", "raw data", "iiwi_data.csv"))
nene <- read.csv(file.path("data", "raw data", "nene_data.csv"))
pueo <- read.csv(file.path("data", "raw data", "pueo_data.csv"))

# check that column names are the same

# datasets with infl length
supportR::diff_check(names(apapane), names(pueo))

# datasets w/ internode
supportR::diff_check(names(iiwi), names(nene))

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
  dplyr::select(-Processed.) # get rid of processed column

apapane <- apapane %>%
  dplyr::rename(any_of(names))

iiwi <- iiwi %>%
  dplyr::rename(any_of(names))

nene <- nene %>%
  dplyr::rename(any_of(names))

# combine all four data sets 
# should end with a data set that is 19 variables long and 947 rows long
join1 <- dplyr::full_join(apapane, pueo)
join2 <- dplyr::full_join(join1, nene)
finaljoin <- dplyr::full_join(join2, iiwi)

# fix ordering of columns
finaljoin <- finaljoin %>%
  dplyr::relocate(int_length_cm, .before = initials)

# write csv
write.csv(x = finaljoin, na = '', row.names = F,
  file.path("data", "PLdata_full.02.17.26.csv"))
 
# End ----
