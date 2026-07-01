###################################
# Brittany Cavazos
# Merge all 4 groups' data into one data set so Jules can fill in missing pieces
###################################

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
join1 <- rbind(apapane, pueo) # both measured infl length
join2 <- rbind(iiwi, nene) # both measured internode length
alltraitdata_v01 <- dplyr::full_join(join1, join2)

# check structure
glimpse(alltraitdata_v01)

# fix ordering of columns
alltraitdata_v02 <- alltraitdata_v01 %>%
  dplyr::relocate(int_length_cm, .before = initials) %>%
  dplyr::rename(country = dwc.countr,
                abbrev_country = idigbio.is,
                longitude = idigbio.lo,
                latitude = idigbio.la,
                year = idigbio.ev)

glimpse(alltraitdata_v02)

# fix dates!

alltraitdata_v03 <- alltraitdata_v02 %>%
  dplyr::mutate(idigbio._1 = coalesce(
    lubridate::ymd(idigbio._1),
    lubridate::mdy(idigbio._1))) %>%
  tidyr::separate_wider_delim(cols = idigbio._1, delim = "-", cols_remove = FALSE,
                              names = c("badyear", "month", "day")) %>%
  tidyr::unite("date", year, month, day, sep = "-") %>%
  dplyr::select(-badyear) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::relocate(date, .before = range)

glimpse(alltraitdata_v03)

# write csv
write.csv(x = alltraitdata_v03, na = '', row.names = F,
  file.path("data", "PLdata_full.02.17.26.csv"))
 
# End ----
