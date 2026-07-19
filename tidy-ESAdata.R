###################################
# Brittany Cavazos
# Clean data set Jules has been filling in this summer to analyse for ESA
###################################

# load libraries
## install.packages("librarian", "textclean")
librarian::shelf(tidyverse, supportR, textclean)

# Make needed folder(s)
dir.create(path = file.path("data", "raw data"), showWarnings = F, recursive = T)

traits_v01 <- read.csv(file.path("data", "raw data", "PLdata_full.07.19.26.csv")) |> 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(nchar(.) == 0 | . == "NA", yes = NA, no = .)))

glimpse(traits_v01)

# fix dates, names, and remove unnecessary columns

traits_v02 <- traits_v01 %>%
  dplyr::filter(!is.na(coreid)) %>%
  dplyr::mutate(idigbio._1 = coalesce(
    lubridate::ymd(idigbio._1),
    lubridate::mdy(idigbio._1))) %>%
  tidyr::separate_wider_delim(cols = idigbio._1, delim = "-", cols_remove = FALSE,
                              names = c("badyear", "month", "day")) %>%
  tidyr::unite("date", idigbio.ev, month, day, sep = "-", remove = FALSE) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::rename(year = idigbio.ev, 
                longitude = idigbio.lo, 
                latitude = idigbio.la,
                country = dwc.countr) %>%
  dplyr::relocate(date, .before = range) %>%
  dplyr::filter(country == "united states" | country == "canada" | country == "netherlands" | country == "norway") %>%
  dplyr::mutate(range = textclean::replace_non_ascii(range),
                range = case_when(country %in% c("canada", "united states") ~ "invasive",
                                  country %in% c("norway", "netherlands") ~ "native")) %>%
  dplyr::select(-badyear, 
                -Column1, 
                -Broken.Link, 
                -Data.Complete., 
                -dwc.scient,
                -dwc.geodet,
                -idigbio.ge,
                -day,
                -pixelpercm, 
                -idigbio._1, 
                -idigbio.is,
                -int_length_Jules,
                -initials,
                -date_collected) 


glimpse(traits_v02)

# write csv
write.csv(x = traits_v02, na = '', row.names = F,
          file.path("data", "PL_traits-tidy.csv"))

# End ----
